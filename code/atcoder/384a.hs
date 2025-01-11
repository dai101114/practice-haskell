main :: IO ()
main = do
  [nStr, c1Str, c2Str] <- words <$> getLine
  s <- getLine
  let n = read nStr :: Int
  let c1 = head c1Str
  let c2 = head c2Str
  let result = map (\ch -> if ch == c1 then ch else c2) s
  putStrLn result
