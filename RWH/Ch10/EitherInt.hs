{-# LANGUAGE FlexibleInstances #-}
instance Functor (Either Int) where
    fmap _ (Left n) = Left n
    fmap f (Right x) = Right (f x)