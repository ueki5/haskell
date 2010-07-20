module Ch07.Toupper_lazy5 where 
import Data.Char(toUpper,isUpper,toLower)

main :: IO ()
-- main = interact ((++) "Your data, in uppercase, is:\n\n" . map toUpper)
main = interact ((++) "Your data, in uppercase, is:\n\n" . map reversal)
    where reversal c = if isUpper c then toLower c else toUpper c
