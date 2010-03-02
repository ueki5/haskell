module Simauti (dayOfWeek) where

-- 島内の式

f, g, h :: Int -> Float
h m = [6.75,2.75,3.25,6.25,1.25,4.25,6.25,2.25,5.25,0.25,3.25,5.25]!!(m - 1)
g 0 = -0.25
g b = fromIntegral (b + c) - if d == 0 then 0.5 else 0
          where (c, d) = b `divMod` 4
f a = [5.875,4.125,2.125,0.125]!!(a `mod` 4)
dayOfWeek :: Int -> Int -> Int -> Int
dayOfWeek y m d = (round (f a + g b + h m) + d) `mod` 7
                    where (a, b) = y `divMod` 100
