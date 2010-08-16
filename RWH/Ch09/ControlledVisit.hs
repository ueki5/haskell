{-# OPTIONS_GHC -cpp #-}
module Ch09.ControlledVisit where
import Control.Monad(filterM,forM,liftM)
import System.Directory(Permissions(..),getModificationTime,getPermissions,getDirectoryContents)
import System.Time(ClockTime(..))
import System.FilePath(takeExtension,(</>))
#if __GLASGOW_HASKELL__ > 608
import Control.OldException(bracket, handle)
#else
import Control.Exception(bracket, handle)
#endif
import System.IO (IOMode(..),hClose,hFileSize, openFile)

import Ch09.RecursiveContents (getRecursiveContents)
import Ch09.BetterPredicate (getFileSize)
data Info = Info {
      infoPath :: FilePath
      , infoPerms :: Maybe Permissions
      , infoSize :: Maybe Integer
      , infoModTime :: Maybe ClockTime
      } deriving (Eq, Ord, Show)

getInfo :: FilePath -> IO Info
-- getInfo path =  do
--           perms <- getPermissions path
--           size <- getFileSize path
--           modified <- getModificationTime path
--           return (Info path (Just perms) size (Just modified))
maybeIO :: IO a -> IO (Maybe a)
maybeIO act = handle (\_ -> return Nothing) (Just `liftM` act)
getInfo path = do
    perms <- maybeIO (getPermissions path)
    size <- maybeIO (bracket (openFile path ReadMode) hClose hFileSize)
    modified <- maybeIO (getModificationTime path)
    return (Info path perms size modified)

traverse :: ([Info] -> [Info]) -> FilePath -> IO [Info]
traverse order path = do
    names <- getUsefulContents path
    contents <- mapM getInfo (path : map (path </>) names)
    liftM concat $ forM (order contents) $ \info -> do
        if isDirectory info && infoPath info /= path
           then traverse order (infoPath info)
           else return [info]
getUsefulContents :: FilePath -> IO [String]
getUsefulContents path = do
    names <- getDirectoryContents path
    return (filter (`notElem` [".",".."]) names)
isDirectory :: Info -> Bool
isDirectory = maybe False searchable . infoPerms
myFilter :: [Info] -> [Info]
myFilter infos = filter myFilter' infos
    where myFilter' :: Info -> Bool
          myFilter' info = case infoSize info of
                             Nothing -> False
                             Just size | size > 100 -> True
                                       | otherwise -> False
