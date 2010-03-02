import Time                         -- Module TimeのMonth型を使うため
                                    -- 復活祭の算法で使用

-- ----------------------------------------------------------------------
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
-- ----------------------------------------------------------------------
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

-- ----------------------------------------------------------------------
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

-- ----------------------------------------------------------------------
-- カレンダーのプログラム

intToString :: Int -> String --カレンダーの日付けを2桁の文字列にする
intToString n = [" 123"!!a, ['0'..'9']!!b] where (a, b) = divMod n 10

intsToString :: [Int] -> Int -> String  --1行(7日分)を文字列にする
intsToString ns ld = concatMap d ns     --concatMapはmapしてappend
        where d x |x <= 0 || x > ld = "   "
                  |otherwise = intToString x ++ " "

monthnames :: [String]              -- 見出しに使う月の名前
monthnames =["January","February","March","April","May","June","July",
             "August","September","October","November","December"]

month :: Int -> String          --21文字に展開した月の名前
month m = expand (monthnames !! (m - 1))

expand :: String -> String      --文字列sの両側に空白を置き21文字に展開する
expand s = let leng = length s  --文字列s自身の長さ
               padlen = (20 - leng) `div` 2         --片側の空白の文字数
             in take 21 (replicate padlen ' ' ++ s ++ repeat ' ')

cal :: Int -> Int -> IO ()      --y年m月のカレンダーを出力
cal y m = do putStrLn head      --見出し(月 年)を出力
             putStrLn (unlines (cc y m))
               where head = expand ((monthnames !! (m - 1)) ++ " " ++ year)
                     year = show y          --西暦年yを文字列に変換

daynames :: String
daynames = " S  M Tu  W Th  F  S "
leap :: Int -> Int              --yがうるう年なら1, 平年なら0を返す
leap y = dif 4 - dif 100 + dif 400
   where dif d = div y d - div y1 d; y1 = y - 1
cc :: Int -> Int -> [String]    --y年m月のカレンダーの文字列を構成
cc y m = let z = 1 - zeller y m 1
             ld = [31,28+leap y,31,30,31,30,31,31,30,31,30,31]!!(m-1)
     in daynames : map (\x-> intsToString x ld) [[d..d+6]|d<-[z,z+7..z+35]]

-- 使い方 cal 2006 1

-- ----------------------------------------------------------------------
-- 1年分のカレンダー

cals :: Int -> IO ()            --y年のカレンダーを出力
cals y = do putStrLn (replicate 30 ' ' ++ show y ++ "\n")   --見出しの出力
            putStrLn (unlines (map unwords mcc))
             where mcc = concatMap mc3 [1,4,7,10]
                   mc3 m = map f (zip3 (mc m) (mc (m + 1)) (mc (m + 2)))
                      where f (a, b, c) = [a, b, c]
                   mc m = month m : cc y m
-- 使い方 cals 2006

-- ----------------------------------------------------------------------
-- 復活祭

-- import Time                         -- Module TimeのMonth型を使うため
easter :: Int -> (Month, Int)
easter y = 
 if n''>31 then (April,n''-31) else (March,n'')     -- E8
  where
    n''= n' + 7 - (d + n') `mod` 7                  -- E7
      where n'= if n < 21 then n + 30 else n        -- E6
            n = 44 - e'
    e'= if e == 25 && g > 11 || e == 24 then e + 1 else e   -- E5
      where e = (11 * g + 20 + z - x) `mod` 30
    d = (5 * y) `div` 4 - x - 10                    -- E4
    x = 3 * c `div` 4 - 12                          -- E3
    z = (8 * c + 5) `div` 25 - 5    -- `div`,`mod`は*,/と同じ優先度
    c = y `div` 100 + 1                             -- E2
    g = y `mod` 19 + 1                              -- E1

-- ----------------------------------------------------------------------
-- 太陽黄経

sind d = sin (d * pi / 180)         --度単位の三角関数
cosd d = cos (d * pi / 180)
tand d = tan (d * pi / 180)
datan x = 180 / pi * atan x

ld d = norm (280.4665 + 0.98564736 * d + 0.0003 * t * t) 360--平均黄経 l(d)
       where t = d / 36525
omegad d = 282.9373 + 0.00004708 * d + 0.0005 * t * t       --近地点黄経 omage(d)
       where t = d / 36525
ed d = 0.016709 - 0.000042 * t      --離心率 e(d)
       where t = d / 36525

norm :: Float -> Integer -> Float   -- 0〜360度に正規化
norm a d = fromInteger (b `mod` d) + c
   where (b,c) = properFraction a   -- b,cはaの整数部と小数部
lon td =                            -- td(JD)における太陽黄経を計算
  norm l 360   --td = julian date of the day time
   where d = td - 2451545.0         -- 2000年1月1日正午(UT)からの日数
         e = ed d                   -- その時点での離心率
         omega = omegad d           -- その時点での近地点黄経
         m = ld d - omega           -- 平均近点離角
         e' = kepler e m            -- 離心近点離角
         kepler e m =               -- Kepler方程式を解く
           f e0
            where m' = m * pi / 180 -- 弧度法に直す
                  e0 = m'           -- Newton法の初期値
                  f e0 =
                   if abs (e0 - e1) < 0.00001 then (e1 * 180 / pi) else (f e1)
                     where e1 = e0 - (e0 - e * sin e0 - m')/(1 - e * cos e0)
         l = v e m + omega          -- 太陽黄経
           where v e m = 2 * datan (sqrt ((1 + e) / (1 - e)) * tand (e'/2))

