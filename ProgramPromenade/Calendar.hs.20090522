module Calendar (cal,month,cc) where

import Zeller

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
