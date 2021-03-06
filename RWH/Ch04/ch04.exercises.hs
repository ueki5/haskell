module Ch04 where
import Data.Char(digitToInt,isDigit)
import Data.List(groupBy,tails)
-- asInt :: String -> Int
-- asInt s = loop 0 s
--     where loop n [] = n
--           loop n (x:xs) = loop (n * 10 + (digitToInt x)) xs
-- asInt' :: String -> Int
-- asInt' ('-':xs) =  (-1) * (asInt' xs)
-- asInt' xs = foldl (\n x -> n * 10 + (digitToInt x)) 0 xs
-- type Ei = Either String Int
-- asInt'' :: String -> Ei
-- asInt'' "A" = Left "aaaaa"
-- asInt'' "1" = Right 1

--asInt
type Ei = Either String Int
asInt :: String -> Ei
asInt ('-':xs) = case (asInt xs) of
                   Right ans -> Right (-1 * ans)
                   Left ans -> Left ans
asInt xs = foldl fldInt (Right 0) xs
fldInt :: Ei -> Char -> Ei
fldInt (Left s) x = Left s
fldInt (Right n) x 
    | isDigit x = Right (n * 10 + (digitToInt x))
    | otherwise = Left ("non-digit '" ++ [x] ++ "'")
--concat
concat' :: [[a]] -> [a]
concat' = foldr (++) []
--takeWrhile
takeWhile' :: (a -> Bool) -> [a] -> [a]
takeWhile' f [] = []
takeWhile' f (x:xs) = case (f x) of
                        False -> []
                        True -> x:(takeWhile' f xs)
takeWhile'' :: (a -> Bool) -> [a] -> [a]
takeWhile'' f xs = foldr takef [] xs
                   where takef x b = case (f x) of
                                     False -> []
                                     True -> x:b
-- groupBy
groupBy' :: (a -> a -> Bool) -> [a] -> [[a]]
groupBy' p xs = foldl (groupByf p) [] xs
groupByf :: (a -> a -> Bool) -> [[a]] -> a -> [[a]]
groupByf p [] y = [[y]]
groupByf p (x:xs) y = if p (last x) y 
                      then (y:x):xs
                      else x:(groupByf p xs y)
tails' :: [a] -> [[a]]
tails' [] = []
tails' xs@(x:xs') = xs:(tails' xs')
myfoldl' :: (a -> b -> a) -> a -> [b] -> a
myfoldl' _ zero [] = zero
myfoldl' step zero (x:xs) = let new = step zero x
                          in seq new (myfoldl' step new xs)
myfoldr' :: (a -> b -> b) -> b -> [a] -> b
myfoldr' _ zero [] = zero
myfoldr' step last (x:xs) = let new = step x last
                          in seq new (myfoldr' step new xs)