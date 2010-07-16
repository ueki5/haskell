module Ch07.Return1 where
import Data.Char(toUpper)

isGreen =
    do putStrLn "Is green your favorite color?"
       inpstr <- getLine
       return ((toUpper . head $ inpstr) == 'Y')