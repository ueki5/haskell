{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE OverlappingInstances #-}
module Ch16.ApplicativeParsec
    (
    module Control.Applicative
    , module Text.ParserCombinators.Parsec
    ) where
import Control.Applicative
import Control.Monad (MonadPlus(..), ap)
-- Applicative�Œ񋟂���邢�����̖��O���B��
import Text.ParserCombinators.Parsec hiding (many, optional, (<|>))

-- ���ׂĂ�Monad�ׂ̈�Applicative�C���X�^���X
-- instance Applicative (GenParser s a) where
--     pure = return
--     (<*>) = ap
--     (*>) :: f a -> f b -> f b
--     (<*) :: f a -> f b -> f a
-- ���ׂĂ�MonadPlus�ׂ̈�Alternative�C���X�^���X
-- instance Alternative (GenParser s a) where
--     empty = mzero
--     (<|>) = mplus
