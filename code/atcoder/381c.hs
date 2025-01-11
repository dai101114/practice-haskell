import Control.Monad

main :: IO ()
main = do
  [_, s] <- replicateM 2 getLine
  print $ (solve s 0 False 0 0) * 2 + 1

solve :: String -> Int -> Bool -> Int -> Int -> Int
solve s c1 b c2 r
  | s == "" = max (min c1 c2) r
  | c == '1' = if not b && c2 == 0 then solve rest (c1 + 1) False c2 r else solve rest 1 False 0 (max (min c1 c2) r)
  | c == '/' = if c1 > 0 && not b then solve rest c1 True c2 r else solve rest 0 False 0 (max (min c1 c2) r)
  | c == '2' = if c1 > 0 && b then solve rest c1 True (c2 + 1) r else solve rest 0 False 0 (max (min c1 c2) r)
  where
    c = head s
    rest = tail s
