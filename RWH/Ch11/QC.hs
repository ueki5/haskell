import Control.Monad
import Test.QuickCheck
import Ch05.Prettify
import Data.List
instance Arbitrary Doc where
    arbitrary = oneof [ return Empty
                        , liftM Char arbitrary
                        , liftM Text arbitrary
                        , return Line
                        , liftM2 Concat arbitrary arbitrary
                        , liftM2 Union arbitrary arbitrary ]
prop_empty_id x = 
    empty <> x == x
  &&
    x <> empty == x
prop_hcat xs = hcat xs == glue xs
    where 
      glue [] = empty
      glue (d:ds) = d <> glue ds
prop_punctuate s xs = punctuate s xs == intersperse s xs
prop_punctuate' s xs = punctuate s xs == combine (intersperse s xs)
    where 
      combine [] = []
      combine [x] = [x]
      combine (Empty: y: xs) = y : (combine xs)
      combine (x: Empty: xs) = x : (combine xs)
      combine (x: y: xs) = x <> y : (combine xs)
prop_char c = char c == Char c
prop_text s = text s == if null s then Empty else Text s
prop_line = line == Line
prop_double d = double d == text (show d)
