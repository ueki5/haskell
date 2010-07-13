module Ch06.Eqclasses where
import Ch06.Naiveeq
class BasicEq a where
    isEqual :: a -> a -> Bool
instance BasicEq Bool where
    isEqual True True = True
    isEqual False False = True
    isEqual _ _ = False
-- instance BasicEq [Char] where
--     isEqual [] [] = True
--     isEqual (x:xs) (y:ys) = x == y && isEqual xs ys
--     isEqual _ _ = False
class BasicEq2 a where
    isEqual2 :: a -> a -> Bool
    isNotEqual2 :: a -> a -> Bool
class BasicEq3 a where
    isEqual3, isNotEqual3 :: a -> a -> Bool
    isEqual3 x y = not (isNotEqual3 x y)
    isNotEqual3 x y = not (isEqual3 x y)
instance BasicEq3 Color where
    isEqual3 Red Red = True
    isEqual3 Green Green = True
    isEqual3 Blue Blue = True
    isEqual3 _ _ = False
