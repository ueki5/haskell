{-# LANGUAGE FunctionalDependencies, MultiParamTypeClasses #-}
module Ch15.MonadHandleIO where

import Ch15.MonadHandle
import qualified System.IO
import System.IO (IOMode(..))
import Control.Monad.Trans (MonadIO(..), MonadTrans(..))
import System.Directory (removeFile)
import Ch15.SafeHello

instance MonadHandle System.IO.Handle IO where
    openFile = System.IO.openFile
    hPutStr = System.IO.hPutStr
    hClose = System.IO.hClose
    hGetContents = System.IO.hGetContents
    hPutStrLn = System.IO.hPutStrLn

class (MonadHandle h m, MonadIO m) => MonadHandleIO h m | m -> h

instance MonadHandleIO System.IO.Handle IO

tidierHello :: (MonadHandleIO h m) => FilePath -> m ()
tidierHello path = do
  safeHello path
  liftIO (removeFile path)
tidyHello :: (MonadIO m, MonadHandle h m) => FilePath -> m ()
tidyHello path = do
  safeHello path
  liftIO (removeFile path)