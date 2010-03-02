import qualified System.IO.UTF8 as U
main = do cs <- U.getContents          
          U.putStrLn $ unlines $ take 3 $ lines cs