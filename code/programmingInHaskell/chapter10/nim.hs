import Data.Char

next :: Int -> Int
next 1 = 2
next 2 = 1

type Board = [Int]

initial :: Board
initial = [5, 4, 3, 2, 1]

finished :: Board -> Bool
finished = all (== 0)

valid :: Board -> Int -> Int -> Bool
valid board row num = board !! (row - 1) >= num

move :: Board -> Int -> Int -> Board
move board row num = [update r n | (r, n) <- zip [1 ..] board]
  where
    update r n = if r == row then n - num else n

putRow :: Int -> Int -> IO ()
putRow row num = do
  putStr (show row)
  putStr ": "
  putStrLn (concat (replicate num "* "))

-- 元の実装
-- putBoard :: Board -> IO ()
-- putBoard [a, b, c, d, e] = do
--   putRow 1 a
--   putRow 2 b
--   putRow 3 c
--   putRow 4 d
--   putRow 5 e

-- 2. putBoard :: Board -> IO () が表示できるボードは列が五つに固定されていました。
--    任意の大きさのボードを表示できるように、この関数を再帰を用いて拡張してください。
--    ヒント：現在の列の番号も引数として取る補助関数を定義しましょう。
-- putBoard' :: Board -> Int -> IO ()
-- putBoard' [] _ = return ()
-- putBoard' (n : ns) row = do
--   putRow row n
--   putBoard' ns (row + 1)

-- putBoard :: Board -> IO ()
-- putBoard b = putBoard' b 1

-- 3. 最初の練習問題と同様に、リスト内包表記と sequence_ を使って、putBoard の拡張版を再実装してください。
putBoard :: Board -> IO ()
putBoard b = sequence_ [putRow row num | (row, num) <- zip [1 ..] b]

getDigit :: String -> IO Int
getDigit prompt = do
  putStr prompt
  x <- getChar
  newline
  if isDigit x
    then
      return (digitToInt x)
    else do
      putStrLn "ERROR: Invalid digit"
      getDigit prompt

newline :: IO ()
newline = putChar '\n'

play :: Board -> Int -> IO ()
play board player = do
  newline
  putBoard board
  if finished board
    then do
      newline
      putStr "Player "
      putStr (show (next player))
      putStrLn " wins!!"
    else do
      newline
      putStr "Player "
      print player
      row <- getDigit "Enter a row number: "
      num <- getDigit "Stars to remove : "
      if valid board row num
        then play (move board row num) (next player)
        else do
          newline
          putStrLn "ERROR: Invalid move"
          play board player

nim :: IO ()
nim = play initial 1
