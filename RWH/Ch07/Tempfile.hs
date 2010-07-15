import System.IO
import System.Directory(getTemporaryDirectory, removeFile)
import System.IO.Error(catch)
import Control.Excelption(finally)

main :: IO ()
main = withTempFile "mytemp.txt" myAction
myAction :: FilePath -> Handle -> IO ()
myAction tmpname temph = 
    do
      putStrLn 