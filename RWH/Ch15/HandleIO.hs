{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Ch15.HandleIO 
    (
     HandleIO
     , Handle
     , runHandleIO
     , openFile
     , hClose
     , hPutStrLn
    ) where
import Control.Monad.Trans(MonadIO(..))
import System.IO (Handle, IOMode(..))
import System.Directory (removeFile)
import qualified System.IO
-- import qualified System.Directory

newtype HandleIO a = HandleIO { runHandleIO :: IO a}
    deriving (Monad)
openFile :: FilePath -> IOMode -> HandleIO Handle
openFile path mode = HandleIO (System.IO.openFile path mode)
hClose :: Handle -> HandleIO ()
hClose = HandleIO . System.IO.hClose
hPutStrLn :: Handle -> String -> HandleIO ()
hPutStrLn h s = HandleIO (System.IO.hPutStrLn h s)
safeHello :: FilePath -> HandleIO ()
safeHello path = do
  h <- openFile path WriteMode
  hPutStrLn h "hello world"
  hClose h
-- removeFile :: FilePath -> HandleIO ()
-- removeFile path = HandleIO (System.Directory.removeFile path)
instance MonadIO HandleIO where
    liftIO = HandleIO
tidyHello :: FilePath -> HandleIO ()
tidyHello path = do
  safeHello path
  HandleIO $ removeFile path