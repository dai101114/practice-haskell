main :: IO ()
main = do
  n <- readLn
  inputs <-
    mapM
      ( \_ -> do
          line <- getLine
          let [tStr, vStr] = words line
          return (read tStr, read vStr)
      )
      [1 .. n]
  let finalWater = simulate inputs
  print finalWater

simulate :: [(Int, Int)] -> Int
simulate tvs =
  let (_, finalWater) = foldl step (0, 0) tvs
   in finalWater
  where
    step :: (Int, Int) -> (Int, Int) -> (Int, Int)
    step (prevTime, water) (t, v) =
      let deltaT = t - prevTime
          water' = max 0 (water - deltaT)
          water'' = water' + v
       in (t, water'')
