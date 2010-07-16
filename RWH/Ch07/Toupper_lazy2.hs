module Ch07.Toupper_lazy2 where 
import System.IO
import Data.Char(toUpper)

main :: IO ()
main = do
       inh <- openFile "input.txt" ReadMode
       ouh <- openFile "output.txt" WriteMode
       inpStr <- hGetContents inh
       hPutStr ouh (map toUpper inpStr)
       hClose inh
       hClose ouh
