mygetLine::IO String
mygetLine = do x <- getChar
               if x == '\n'
                  then return []
                  else do xs <- mygetLine
                          return (x:xs)
myputStr::String -> IO ()
myputStr [] = return ()
myputStr (x:xs) = do putChar x
                     myputStr xs
myputStrLn::String -> IO ()
myputStrLn xs = do myputStr xs
                   putChar '\n'
strlen::String -> Int
strlen [] = 0
strlen (x:xs) = 1 + strlen xs
strLen::IO ()
strLen = do myputStr "input string:"
            xs <- mygetLine
            myputStr "The string has "
            myputStr $ show $ strlen xs
            myputStrLn " characters!"
beep::IO ()
beep = myputStr "\BEL"
cls::IO ()
cls = myputStr "\ESC[2J"
type Pos = (Int, Int)
goto::Pos -> IO ()
goto (x, y) = myputStr ("\ESC[" ++ show y ++ ";" ++ show x ++ "H")
