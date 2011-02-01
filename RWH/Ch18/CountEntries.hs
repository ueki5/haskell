-- file: ch18/CountEntries.hs
module Ch18.CountEntries (listDirectory, countEntriesTrad) where

import System.Directory (doesDirectoryExist, getDirectoryContents)
import System.FilePath ((</>))
import Control.Monad (forM, liftM)

listDirectory :: FilePath -> IO [String]
listDirectory = liftM (filter notDots) . getDirectoryContents
    where notDots p = p /= "." && p /= ".."

countEntriesTrad :: FilePath -> IO [(FilePath, Int)]
countEntriesTrad path = do
  contents <- listDirectory path
  rest <- forM contents $ \name -> do
            let newName = path </> name
            isDir <- doesDirectoryExist newName
            if isDir
              then countEntriesTrad newName
              else return []
  return $ (path, length contents) : concat rest

-- myListDirectory :: FilePath -> IO [String]
-- myListDirectory path = return [path,path,path]
-- -- myCountEntries :: FilePath -> IO [(FilePath, Int)]
-- myCountEntries :: FilePath -> IO [FilePath]
-- myCountEntries path = do
--   contents <- myListDirectory path
--   rest <- forM contents $ \name -> do
--                         return name
-- --   return $ (path, length contents) : concat rest
--   return rest
