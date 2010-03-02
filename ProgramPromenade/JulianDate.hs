module JulianDate (julianDate) where

-- Julian Date

mon0, mon1 :: [Int]         -- 年初から前月末日までの日数
mon0 = [0, 31, 59, 90, 120, 151, 181, 212, 243, 273, 304, 334]    -- 平年
mon1 = [0, 31, 60, 91, 121, 152, 182, 213, 244, 274, 305, 335]    -- うるう年

gleap, jleap :: Int -> Bool         -- Gregorian, Julian暦のうるう年に真を返す
gleap y = if y `mod` 100 == 0 then y `mod` 400 == 0 else y `mod` 4 == 0
jleap y = y `mod` 4 == 0

julianDate :: Int -> Int -> Int -> Int
julianDate y m d =                   -- aからgは途中の値
 let a = (y + 4712) * 365 
     b = (y + 4712 + 3) `div` 4
     c = if y > 1601 then y' `div` 400 - y' `div` 100 else 0
           where y' = y - 1601
     e = if [y,m,d] >= [1582,10,15] then -10 else 0
     f = (if leap y then mon1 else mon0) !! (m - 1)
           where leap = if y > 1600 then gleap else jleap
     g = d - 1
  in a + b + c + e + f + g           -- 最後の結果
