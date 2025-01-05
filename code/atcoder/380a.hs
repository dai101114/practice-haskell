main :: IO ()
main = do
  n <- getLine
  let count = sum $ map (\x -> read [x] :: Int) n
  putStrLn $ if count == 14 then "Yes" else "No"
