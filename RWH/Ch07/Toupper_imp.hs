module Ch07.Toupper_imp where 
import System.IO
import Data.Char(toUpper)

main :: IO ()
main = do
       inh <- openFile "input.txt" ReadMode
       ouh <- openFile "output.txt" WriteMode
       mainloop inh ouh
       hClose inh
       hClose ouh
mainloop :: Handle -> Handle -> IO ()
mainloop inh ouh = 
    do
    ineof <- hIsEOF inh
    if ineof 
        then return ()
        else do inpStr <- hGetLine inh
                hPutStrLn ouh (map toUpper inpStr)
                mainloop inh ouh