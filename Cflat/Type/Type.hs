module Cflat.Type.Type where

data Type =  TpInt Int
              deriving (Eq, Ord, Show)
data Operator = Plus
             | Minus
             | Mult
             | Div
             | Power
              deriving (Eq,Show)
data Formula = Tp Type
                      | Op Operator Formula Formula
              deriving (Eq,Show)
