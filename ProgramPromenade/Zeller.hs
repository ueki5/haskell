module Zeller (zeller) where

-- Zellerの合同式

zeller y m d = if m < 3 then z (y - 1) (m + 10) else z y (m - 2)
  where z y' m' = (floor(fromIntegral m' * 2.6 - 0.2) + d + b 
         + floor(fromIntegral b / 4) + floor(fromIntegral a / 4) + 5 * a) `mod` 7
             where (a, b) = y' `divMod` 100

{-
zeller y m d = if m < 3 then z (y - 1) (m + 10) else z y (m - 2)
  where z y' m' = ((m' * 26 - 2) `div` 10 + d + b + b `div` 4 + a `div` 4 + 5 * a) 
                  `mod` 7
             where (a, b) = y' `divMod` 100
-}
