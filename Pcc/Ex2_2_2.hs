module Pcc.Ex2_2_2 where 
import Data.List
import Pcc.Comb
data Work = Work {start::Int, end::Int}
            deriving (Show, Eq, Ord)
input = [Work 1 2 ,Work 2 3, Work 2 5, Work 6 9, Work 10 10, Work 3 3, Work 4 5]
solve :: [Work] -> Int
solve ws = length $ head $ reverse $ (sortBy lendiff (solve' ws))
lendiff xs ys |length xs <  length ys = LT
              |length xs == length ys = EQ
              |length xs >  length ys = GT
solve' :: [Work] -> [[Work]]
solve' ws = filter notDuplicate (subsequences ws)
notDuplicate :: [Work] -> Bool
notDuplicate ws = not (any duplicate' ws')
  where
    ws' = splits div1 ws
duplicate :: Work -> Work -> Bool
duplicate (Work s1 e1) (Work s2 e2) | s1 < s2 && e1 < s2 = False
                                    | e2 < s1 && e2 < e1 = False
                                    | otherwise = True
duplicate' :: (Work ,[Work]) -> Bool
duplicate' (w, ws) = any (duplicate w) ws