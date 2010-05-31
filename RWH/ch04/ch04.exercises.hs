module Ch04 where
import Data.Char(digitToInt,isDigit)
import Data.List(groupBy)
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
--groupBy
-- groupBy' :: (a -> a -> Bool) -> [a] -> [[a]]
