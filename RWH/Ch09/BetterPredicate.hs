module Ch09.BetterPredicate where
import Control.Monad(filterM)
import System.Directory(Permissions(..),getModificationTime,getPermissions)
import System.Time(ClockTime(..))
import System.FilePath(takeExtension)
import Control.Exception(bracket, handle)
import System.IO (IOMode(..),hClose,hFileSize, openFile)

import Ch09.RecursiveContents (getRecursiveContents)

type Predicate = FilePath 
    -> Permissions
    -> Maybe Integer
    -> ClockTime
    -> Bool
