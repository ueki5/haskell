{-# LANGUAGE TypeSynonymInstances,OverlappingInstances #-}
module Ch16.ApplicativeParsec
    (
    module Control.Applicative
    , module Text.ParserCombinators.Parsec
    ) where
import Control.Applicative
import Control.Monad (MonadPlus(..), ap)
-- Applicative‚Å’ñ‹Ÿ‚³‚ê‚é‚¢‚­‚Â‚©‚Ì–¼‘O‚ð‰B‚·
import Text.ParserCombinators.Parsec hiding (many, optional, (<|>))

instance Alternative (GenParser s a) where
    empty = mzero
    (<|>) = mplus
instance Applicative (GenParser s a) where
    pure = return
    (<*>) = ap
