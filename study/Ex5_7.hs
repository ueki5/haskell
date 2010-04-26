module Ex5_7 where
import Prelude hiding (replicate)
fun1 :: Int -> Int
fun1 n = foldl (+) 0 [x * x | x <- [1..n]]
replicate :: Int -> a -> [a]
replicate n a = [a | i <- [1 .. n]]
pyths :: Int -> [(Int, Int, Int)]
pyths n = [(a, b, c) | a <- [1 .. n],
                       b <- [1 .. n],
                       c <- [1 .. n],
           a^2 + b^2 == c^2]
perfects :: Int -> [Int]
perfects n = [n]
yakusu :: Int -> [[Int]]
yakusu n = [i:[n `div` i]  | i <- [1 .. n], i /= 1 && i /= n && n `mod` i == 0]
l5_7_5 :: [(Int,Int)]
l5_7_5 = [(x, y)|x <- [1,2,3],y <- [4,5,6]]
