module Ch07.Toupper_lazy3 where 
import System.IO
import Data.Char(toUpper)

main :: IO ()
main = do
       inpStr <- readFile "input.txt"
       writeFile "output.txt" (map toUpper inpStr)
