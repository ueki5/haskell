module Ch07.Actions3 where
import Data.Char(toUpper,isUpper,toLower)

func :: String -> IO String
func str = do 
  putStrLn str
  return str
main = getLine 
        >>= mrevert
--         >>= myToUpper4
--        >>= myToUpper
--        >>= return . (map toUpper)
       >>= putStrLn
myToUpper :: String -> IO String
myToUpper str = return (map toUpper str)
myToUpper2 str = (return . (map toUpper)) str
-- myToUpper3 :: String -> IO String
myToUpper3 str = return (map toUpper str)
myToUpper4 :: (Monad m) =>  String -> m String
myToUpper4 = return . map toUpper
revert :: String -> String
revert [] = []
revert (c:cs) = if isUpper c then (toLower c):(revert cs)
                             else (toUpper c):(revert cs)
mrevert :: (Monad m) => String -> m String
mrevert = return . revert