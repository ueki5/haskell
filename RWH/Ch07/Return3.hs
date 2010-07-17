module Ch07.Return3 where
import Data.Char(toUpper)

returnTest :: IO ()
returnTest = 
    do one <- return 1
       let two = 2
       putStrLn $ show (one + two)