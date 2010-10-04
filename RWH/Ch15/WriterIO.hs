{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances #-}
module Ch15.WriterIO where

import System.IO --(IOMode(..))
import Control.Monad.Writer
import Ch15.MonadHandleIO
import Ch15.MonadHandle
import Ch15.SafeHello
data Event = Open FilePath IOMode
           | Put String String
           | Close String
           | GetContents String
             deriving (Show)
newtype WriterIO a = W { runW :: Writer [Event] a}
    deriving (Monad, MonadWriter [Event])
runWriterIO :: WriterIO a -> (a, [Event])
runWriterIO = runWriter . runW
    
instance MonadHandle FilePath WriterIO where
    openFile path mode = tell [Open path mode] >> return path
    hPutStr h str = tell [Put h str]
    hClose h = tell [Close h]
    hGetContents h = tell [GetContents h] >> return ""
