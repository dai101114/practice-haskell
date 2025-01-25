import Data.Char -- for isDigit
import Data.List -- for transpose
import System.IO -- for buffering

------------------------------------------------------------
-- ターミナルをクリア＆カーソル移動する簡易関数

cls :: IO ()
cls = putStr "\ESC[2J" -- 全画面クリア

goto :: (Int, Int) -> IO ()
goto (x, y) = putStr $ "\ESC[" ++ show y ++ ";" ++ show x ++ "H"

------------------------------------------------------------
-- 定数や型定義

-- 格子のサイズ (3x3)
size :: Int
size = 3

-- プレイヤーの型
data Player = O | B | X
  deriving (Eq, Ord, Show)

-- 格子はプレイヤーの値 (Player) の二次元リスト
type Grid = [[Player]]

------------------------------------------------------------
-- プレイヤーの切り替え
next :: Player -> Player
next O = X
next B = B
next X = O

------------------------------------------------------------
-- 空の格子の生成
empty :: Grid
empty = replicate size (replicate size B)

------------------------------------------------------------
-- 格子がすべて埋まっているか判定
full :: Grid -> Bool
full = notElem B . concat

------------------------------------------------------------
-- 次に打つべきプレイヤーを判定
-- O が先攻の場合、O と X の数を比較して決める
turn :: Grid -> Player
turn g = if os <= xs then O else X
  where
    ps = concat g
    os = length (filter (== O) ps)
    xs = length (filter (== X) ps)

------------------------------------------------------------
-- 与えられたプレイヤーが、行・列・斜めを占有していれば勝ち
wins :: Player -> Grid -> Bool
wins p g = any line (rows ++ cols ++ dias)
  where
    line = all (== p)
    rows = g
    cols = transpose g
    dias = [diag g, diag (map reverse g)]

-- 左上→右下の対角線を取り出す
diag :: Grid -> [Player]
diag g = [g !! n !! n | n <- [0 .. size - 1]]

-- O または X が勝利しているか
won :: Grid -> Bool
won g = wins O g || wins X g

------------------------------------------------------------
-- 格子を画面に表示する関連関数

-- 格子全体を表示
putGrid :: Grid -> IO ()
putGrid =
  putStrLn . unlines . concat . interleave bar . map showRow
  where
    bar = [replicate ((size * 4) - 1) '-']

-- 1行分を文字列に変換
showRow :: [Player] -> [String]
showRow =
  beside . interleave bar . map showPlayer
  where
    beside = foldr1 (zipWith (++))
    bar = replicate 3 "|"

-- プレイヤーを文字列に変換
showPlayer :: Player -> [String]
showPlayer O = ["   ", " O ", "   "]
showPlayer B = ["   ", "   ", "   "]
showPlayer X = ["   ", " X ", "   "]

-- リストの要素の間に指定した要素を挟み込む
interleave :: a -> [a] -> [a]
interleave _ [] = []
interleave _ [y] = [y]
interleave x (y : ys) = y : x : interleave x ys

------------------------------------------------------------
-- 有効な指し手か判定
valid :: Grid -> Int -> Bool
valid g i =
  0 <= i && i < size ^ 2 && concat g !! i == B

-- 指し手を適用して新たな格子を返す(リストで返す)
move :: Grid -> Int -> Player -> [Grid]
move g i p =
  [chop size (xs ++ [p] ++ ys) | valid g i]
  where
    (xs, B : ys) = splitAt i (concat g)

-- リストを指定した長さごとに区切って二次元リストにする
chop :: Int -> [a] -> [[a]]
chop _ [] = []
chop n xs = take n xs : chop n (drop n xs)

------------------------------------------------------------
-- ユーザから自然数を読み込む
getNat :: String -> IO Int
getNat prompt = do
  putStr prompt
  xs <- getLine
  if xs /= [] && all isDigit xs
    then return (read xs)
    else do
      putStrLn "ERROR: Invalid number"
      getNat prompt

------------------------------------------------------------
-- (人間 vs 人間) の三目並べ

tictactoe :: IO ()
tictactoe = run empty O

run :: Grid -> Player -> IO ()
run g p = do
  cls
  goto (1, 1)
  putGrid g
  run' g p

run' :: Grid -> Player -> IO ()
run' g p
  | wins O g = putStrLn "Player O wins!\n"
  | wins X g = putStrLn "Player X wins!\n"
  | full g = putStrLn "It's a draw!\n"
  | otherwise = do
      i <- getNat (prompt p)
      case move g i p of
        [] -> do
          putStrLn "ERROR: Invalid move"
          run' g p
        [g'] -> run g' (next p)

prompt :: Player -> String
prompt p = "Player " ++ show p ++ ", enter your move: "

------------------------------------------------------------
-- ゲームの木の定義と生成

data Tree a = Node a [Tree a]
  deriving (Show)

board :: Grid
board = [[O, B, B], [X, X, O], [X, O, B]]

-- 与えられた局面とプレイヤーからゲームの木を生成
gametree :: Grid -> Player -> Tree Grid
gametree g p =
  Node g [gametree g' (next p) | g' <- moves g p]

-- 与えられた局面で有効な手をすべて生成
moves :: Grid -> Player -> [Grid]
moves g p
  | won g = []
  | full g = []
  | otherwise = concat [move g i p | i <- [0 .. (size ^ 2 - 1)]]

------------------------------------------------------------
-- 木の深さを制限して枝刈りする

prune :: Int -> Tree a -> Tree a
prune 0 (Node x _) = Node x []
prune n (Node x ts) = Node x [prune (n - 1) t | t <- ts]

-- 今回は 3x3 の場合、最大深さ = 9
depth :: Int
depth = 9

------------------------------------------------------------
-- ミニマックス法で木にラベルをつける

minimax :: Tree Grid -> Tree (Grid, Player)
minimax (Node g [])
  | wins O g = Node (g, O) []
  | wins X g = Node (g, X) []
  | otherwise = Node (g, B) []
minimax (Node g ts)
  | turn g == O = Node (g, minimum ps) ts'
  | turn g == X = Node (g, maximum ps) ts'
  where
    ts' = map minimax ts
    ps = [p | Node (_, p) _ <- ts']

------------------------------------------------------------
-- ミニマックス法にもとづいて最善手を選ぶ

bestmove :: Grid -> Player -> Grid
bestmove g p =
  head [g' | Node (g', p') _ <- ts, p' == best]
  where
    tree = prune depth (gametree g p)
    Node (_, best) ts = minimax tree

------------------------------------------------------------
-- (人間 vs コンピュータ) 版の三目並べ

main :: IO ()
main = do
  hSetBuffering stdout NoBuffering
  play empty O -- Oを人間にして先攻

play :: Grid -> Player -> IO ()
play g p = do
  cls
  goto (1, 1)
  putGrid g
  play' g p

play' :: Grid -> Player -> IO ()
play' g p
  | wins O g = putStrLn "Player O wins!\n"
  | wins X g = putStrLn "Player X wins!\n"
  | full g = putStrLn "It's a draw!\n"
  | p == O = do
      i <- getNat (prompt p)
      case move g i p of
        [] -> do
          putStrLn "ERROR: Invalid move"
          play' g p
        [g'] -> play g' (next p)
  | p == X = do
      putStr "Player X is thinking... "
      -- \$! で最善手の計算を先に強制評価
      (play $! bestmove g p) (next p)
