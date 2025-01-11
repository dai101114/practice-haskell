main :: IO ()
main = do
  s <- getLine
  putStrLn $ if solve s [] then "Yes" else "No"

solve :: String -> String -> Bool
solve "" _ = True
solve [_] _ = False
solve (s1 : s2 : s) xs
  | len `mod` 2 == 0 = s1 == s2 && not (s1 `elem` xs) && solve s (s1 : xs)
  | otherwise = False
  where
    len = length s
