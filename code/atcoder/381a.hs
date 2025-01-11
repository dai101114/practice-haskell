import Control.Monad

main :: IO ()
main = do
  [_, s] <- replicateM 2 getLine
  putStrLn $ if is1122String s then "Yes" else "No"

is1122String :: String -> Bool
is1122String "/" = True
is1122String s
  | len == 1 = False
  | len `mod` 2 /= 0 = head s == '1' && last s == '2' && is1122String (init $ tail s)
  | otherwise = False
  where
    len = length s
