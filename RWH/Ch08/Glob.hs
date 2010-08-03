module Ch08.Glob 
    (
     namesMatching
    ) where
import System.Directory 
    (
     doesDirectoryExist
     ,doesFileExist
     ,getCurrentDirectory
     ,getDirectoryContents
    )
import System.FilePath 
    (
     dropTrailingPathSeparator
     ,splitFileName
     ,(</>)
    )
-- import Control.Exception (handle)
import Control.OldException (handle)
import Control.Monad (forM)
import Ch08.GlobRegex (matchesGlob)

namesMatching pat 
    | not (isPattern pat) = do
      exists <- doesNameExists pat
      return (if exists then [pat] else [])
    | otherwise = do
      case splitFileName pat of
        ("", baseName) -> do
           curDir <- getCurrentDirectory
           listMatches curDir baseName
        (dirName, baseName) -> do
           dirs <- if isPattern dirName
                   then namesMatching (dropTrailingPathSeparator dirName)
                   else return [dirName]
           let listDir = if isPattern baseName
                         then listMatches
                         else listPlain
           pathNames <- forM dirs $ \dir -> do
                           baseNames <- listDir dir baseName
                           return (map (dir </>) baseNames)
           return (concat pathNames)
isPattern :: String -> Bool
isPattern = any (`elem` "[*?")
doesNameExists :: FilePath -> IO Bool
doesNameExists name = do
    fileExists <- doesFileExist name
    if fileExists
      then return True
      else doesDirectoryExist name
listPlain :: FilePath -> String -> IO [String]
listPlain dirName baseName = do
    exists <- if null baseName
              then doesDirectoryExist dirName
              else doesNameExists (dirName </> baseName)
    return (if exists then [baseName] else [])
-- listMatches dirName pat = do
--     dirName' <- if null dirName
--                 then getCurrentDirectory
--                 else return dirName
--     names <- getDirectoryContents dirName'
--     let names' = if isHidden pat
--                  then filter isHidden names
--                  else filter (not . isHidden) names
--     return (filter (`matchesGlob` pat) names')
listMatches dirName pat = do
    dirName' <- if null dirName
                then getCurrentDirectory
                else return dirName
    handle (const (return [])) $ do
        names <- getDirectoryContents dirName'
        let names' = if isHidden pat
                     then filter isHidden names
                     else filter (not . isHidden) names
        return (filter (`matchesGlob` pat) names')
isHidden ('.':_) = True
isHidden _ = False