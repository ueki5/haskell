{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE OverlappingInstances #-}
module Ch16.ApplicativeParsec
    (
    module Control.Applicative
    , module Text.ParserCombinators.Parsec
    ) where
import Control.Applicative
import Control.Monad (MonadPlus(..), ap)
-- Applicativeで提供されるいくつかの名前を隠す
import Text.ParserCombinators.Parsec hiding (many, optional, (<|>))

-- すべてのMonadの為のApplicativeインスタンス
-- instance Applicative (GenParser s a) where
--     pure = return
--     (<*>) = ap
--     (*>) :: f a -> f b -> f b
--     (<*) :: f a -> f b -> f a
-- すべてのMonadPlusの為のAlternativeインスタンス
-- instance Alternative (GenParser s a) where
--     empty = mzero
--     (<|>) = mplus
