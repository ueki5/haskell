module Ch07.Toupper_lazy5 where 
import Data.Char(toUpper)

main :: IO ()
main = interact ((++) "Your data, in uppercase, is:\n\n" . map toUpper)
