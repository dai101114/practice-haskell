import qualified Data.Set as Set

main :: IO ()
main = do
  inputs <- words <$> getLine
  let uniqueCount = Set.size $ Set.fromList inputs
  putStrLn $ if uniqueCount == 2 then "Yes" else "No"
