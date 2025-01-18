import qualified Control.Monad
import Data.Char
import System.IO

-- 1. リスト内包表記とプレリュード関数 sequence_ :: [IO a] -> IO () を用いて、
--    putStr :: String -> IO () を再定義してください。
myputStr :: String -> IO ()
myputStr xs = sequence_ [putChar x | x <- xs]

-- 4. 指定した数だけキーボードから数字を読み取り、それらの和を表示する
--    adder :: IO () を定義してください。数字は行ごとに一つずつ入力するものとします。
--    以下に使用例を示します。
--      > adder
--      How many numbers? 5
--      1
--      3
--      5
--      7
--      9
--      The total is 25
--    ヒント：「現在の合計」と「あといくつ数字を読み込むべきか」を引数として取る補助関数を定義しましょう。プレリュード関数 read と show を使う必要があるかもしれません。

adder :: IO ()
adder = do
  putStr "How many numbers? "
  num <- getLine
  total <- calculate 0 (read num)
  putStrLn ("The total is " ++ show total)

calculate :: Int -> Int -> IO Int
calculate total rest = do
  if rest <= 0
    then
      return total
    else do
      n <- getLine
      calculate (total + read n) (rest - 1)

-- 5. 関数 sequence :: [IO a] -> IO [a] は、アクションのリストを実行した後、
--    結果の値をリストとして返します。この関数を用いて adder を再実装してください。
adder' :: IO ()
adder' = do
  putStr "How many numbers? "
  num <- getLine
  nums <- sequence [getNum | _ <- [1 .. (read num)]]
  putStrLn ("The total is " ++ show (sum nums))

getNum :: IO Int
getNum = do
  read <$> getLine

-- 6. getCh を用いて、アクション readLine :: IO String を定義してください。
--    このアクションは getLine に似ていますが、文字を消すために消去キーが利用できます。
--    ヒント：消去文字は '\DEL' 、一文字後退させる制御文字は '\b' です。

-- | 一文字だけ入力を受け取る（エコーしない）
getCh :: IO Char
getCh = do
  -- エコーをオフ
  hSetEcho stdin False
  c <- getChar
  -- エコーをオンに戻す
  hSetEcho stdin True
  return c

-- | 削除キーが使える readLine
readLine :: IO String
readLine = loop []
  where
    loop :: String -> IO String
    loop xs = do
      c <- getCh
      case c of
        -- Enter（改行）の場合は入力完了
        '\n' -> do
          putChar c -- 改行を表示
          return (reverse xs)

        -- Delete（バックスペース）の場合
        '\DEL' ->
          if null xs
            -- これまでの文字列が空なら何もせず継続
            then loop xs
            else do
              -- 画面上で一文字消す
              putStr "\b \b"
              loop (tail xs)

        -- 上記以外の文字はそのまま出力して続行
        _ -> do
          putChar c
          loop (c : xs)
