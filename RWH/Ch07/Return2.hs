module Ch07.Return2 where
import Data.Char(toUpper)

isYes :: String -> Bool
isYes str = (toUpper . head $ str) == 'Y'
isGreen =
    do putStrLn "Is green your favorite color?"
       inpstr <- getLine
       return (isYes inpstr)