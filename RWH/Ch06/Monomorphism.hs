--{-# LANGUAGE NoMonomorphismRestriction #-}
module Monomorphism where
-- myShow = show
myShow2 value = show value
myShow3 :: (Show a) => a -> String
myShow3 = show