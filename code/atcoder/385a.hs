main :: IO ()
main = do
  [a, b, c] <- map read . words <$> getLine
  putStrLn $ if canDivide a b c then "Yes" else "No"

canDivide :: Int -> Int -> Int -> Bool
canDivide a b c =
  (a == b + c) || (b == a + c) || (c == a + b) || (a == b && b == c)
