main :: IO ()
main = do
  n <- getLine
  let (s1, s2) = solve n
  putStrLn $ s1 ++ " " ++ s2

solve :: String -> (String, String)
solve s = (tail s ++ [head s], [last s] ++ init s)
