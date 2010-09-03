import Control.Monad
import Test.QuickCheck
import Ch11.Prettify2
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

prop_char c = char c == Char c
prop_text s = text s == if null s then Empty s else Text s
                                