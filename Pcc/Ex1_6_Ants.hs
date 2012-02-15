-- 長さLcmの竿の上を
-- n匹のアリが
-- 毎秒１cmのスピード
-- ２匹のアリが出会うと、それぞれ逆の方向へ
-- すべてのアリが竿から落ちるまでにかかる
--   最小の時間と
--   最大の時間を求めよ
-- 1 <= L <= 10^6
-- 1 <= n <= 10^6
-- 0 <= xi <= L
module Pcc.Ex1_6_Ants where
ants = [5, 3, 20, 68, 80, 99]::[Int]
stageLength = 100
solve :: [Int] -> (Int ,Int)
solve ants = (minTime, maxTime)
  where minTime = foldr max 0 $ map minWalkTime ants
        maxTime = foldr max 0 $ map maxWalkTime ants 
        minWalkTime :: Int -> Int
        minWalkTime p | p < stageLength - p = p
                      | otherwise           = stageLength - p
        maxWalkTime :: Int -> Int
        maxWalkTime p | p < stageLength - p = stageLength - p
                      | otherwise           = p
