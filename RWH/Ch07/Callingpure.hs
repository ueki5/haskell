module Ch07.Callingpure where
name2reply :: String -> String
name2reply name =
    "Pleased to meet you, " ++ name ++ ".\n" ++
    "Your name contains " ++ charcount ++ " characters."
    where charcount = show (length name)
main = do
    putStrLn "Greeting once again. what is your name?"
    inpStr <- getLine
    let outStr = name2reply inpStr
    putStrLn outStr