module Ch07.Toupper_lazy4 where 
import Data.Char(toUpper)

main :: IO ()
main = interact (map toUpper)
