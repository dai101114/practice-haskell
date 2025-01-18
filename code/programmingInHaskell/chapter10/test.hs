-- type IO = World -> World
-- type IO a = World -> (a,World)

-- getChar
-- putChar

act :: IO (Char, Char)
act = do
  x <- getChar
  getChar
  y <- getChar
  return (x, y)

getLine :: IO String
getLine = do
  x <- getChar
  if x == '\n'
    then return []
    else do
      xs <- Main.getLine
      return (x : xs)

putStr :: String -> IO ()
putStr [] = return ()
putStr (x : xs) = do
  putChar x
  Main.putStr xs

putStrLn :: String -> IO ()
putStrLn xs = do
  Main.putStr xs
  putChar '\n'

strlen :: IO ()
strlen = do
  Main.putStr "Enter a string: "
  xs <- Main.getLine
  Main.putStr "The string has "
  Main.putStr (show (length xs))
  Main.putStrLn " characters"
