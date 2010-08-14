{-# OPTIONS_GHC -cpp #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
module Ch09.BetterPredicate where
import Control.Monad(filterM)
import System.Directory(Permissions(..),getModificationTime,getPermissions)
import System.Time(ClockTime(..))
import System.FilePath(takeExtension)
#if __GLASGOW_HASKELL__ > 608
import Control.OldException(bracket, handle)
#else
import Control.Exception(bracket, handle)
#endif
import System.IO (IOMode(..),hClose,hFileSize, openFile)

import Ch09.RecursiveContents (getRecursiveContents)

type InfoP a = FilePath
             -> Permissions
             -> Maybe Integer
             -> ClockTime
             -> a
type PathP = InfoP FilePath
type PermP = InfoP Permissions
type SizeP = InfoP Integer
type TimeP = InfoP ClockTime

pathP path _ _ _ = path
permP _ perms _ _ = perms
sizeP _ _ (Just size) _ = size
sizeP _ _ Nothing _ = -1
timeP _ _ _ time = time
liftP' :: (a -> b -> c) -> InfoP a -> InfoP b -> InfoP c
liftP' q f g w x y z = f w x y z `q` g w x y z
liftP :: (a -> b -> c) -> InfoP a -> b -> InfoP c
-- liftP q f k w x y z = f w x y z `q` k
liftP q f k = liftP' q f (constP k)
constP :: a -> InfoP a
constP k _ _ _ _ = k
equalP :: (Eq a) => InfoP a -> a -> InfoP Bool
-- equalP f k w x y z = f w x y z == k
equalP = liftP (==)
greaterP,lesserP :: (Ord a) => InfoP a -> a -> InfoP Bool
greaterP = liftP (>)
lesserP = liftP (<)
simpleAndP :: InfoP Bool -> InfoP Bool -> InfoP Bool
-- simpleAndP f g w x y z = f w x y z && g w x y z
simpleAndP = liftP' (&&)
andP = liftP' (&&)
orP = liftP' (||)


-- type Predicate = FilePath 
--     -> Permissions
--     -> Maybe Integer
--     -> ClockTime
--     -> Bool

getFileSize :: FilePath -> IO (Maybe Integer)
simpleFileSize path = do
  h <- openFile path ReadMode
  size <- hFileSize h
  hClose h
  return (Just size)
saferFileSize path = handle (\_ -> return Nothing) $ do
  h <- openFile path ReadMode
  size <- hFileSize h
  hClose h
  return (Just size)
getFileSize path = handle (\_ -> return Nothing) $ do
  bracket (openFile path ReadMode) hClose $ \h -> do
    size <- hFileSize h
    return (Just size)
betterFind :: InfoP Bool -> FilePath -> IO [FilePath]
betterFind p path = getRecursiveContents path >>= filterM check
  where check name = do
          perms <- getPermissions name
          size <- getFileSize name
          modified <- getModificationTime name
          return (p name perms size modified)

myTest :: InfoP Bool
-- myTest path _ (Just size) _ = takeExtension path == ".cpp" && size > 1
-- myTest _ _ _ _ = False
-- myTest = (liftPath takeExtension `equalP` ".cpp") `andP` (sizeP `greaterP` 1)
myTest = liftPath takeExtension ==? ".cpp" &&? sizeP >? 1
liftPath :: (FilePath -> a) -> InfoP a
liftPath f w _ _ _ = f w
(==?) = equalP
infix 4 ==?
(>?) = greaterP
infix 4 >?
(<?) = lesserP
infix 4 <?
(&&?) = andP
infixr 3 &&?
(||?) = orP
infixr 2 ||?

