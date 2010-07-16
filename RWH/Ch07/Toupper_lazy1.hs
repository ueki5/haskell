module Ch07.Toupper_lazy1 where 
import System.IO
import Data.Char(toUpper)

main :: IO ()
main = do
       inh <- openFile "input.txt" ReadMode
       ouh <- openFile "output.txt" WriteMode
       inpStr <- hGetContents inh
       let result = processData inpStr
       hPutStr ouh result
       hClose inh
       hClose ouh
processData :: String -> String
processData = map toUpper
