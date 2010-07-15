module Ch07.Basicio where
main = do
  putStrLn "Greetings! What is your name?"
  inpStr <- getLine
  putStrLn $ "Welcome to Haskell," ++ inpStr ++ "!"