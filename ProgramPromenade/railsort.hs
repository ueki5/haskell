
decMergen, incMergen    :: [[Int]] -> [Int]     -- 整数のリストのリストをもらいリストを返す
decMergen (xs0:xs1:[])  = decMerge2 xs0 xs1     -- リストが2つのとき マージ
decMergen (xs0:xs1:xss) = decMergen ((decMerge2 xs0 xs1):xss)   -- 3つ以上のとき
incMergen (xs0:xs1:[])  = incMerge2 xs0 xs1
incMergen (xs0:xs1:xss) = incMergen ((incMerge2 xs0 xs1):xss)

decMerge2, incMerge2    :: [Int]->[Int]->[Int]  -- 2つのリストのマージ Curry化
decMerge2 [] ys         = ys                    -- 前のリストが終った
decMerge2 (x:xs) []     = x:xs                  -- 後のリストが終った
decMerge2 (x:xs)(y:ys)
 | x>y                  = x:(decMerge2 xs (y:ys))     -- 小さいほうを出力へ
 | otherwise            = y:(decMerge2 (x:xs) ys)
incMerge2 [] ys         = ys
incMerge2 (x:xs) []     = x:xs
incMerge2 (x:xs)(y:ys)
 | x<y                  = x:(incMerge2 xs (y:ys))
 | otherwise            = y:(incMerge2 (x:xs) ys)

decSort, incSort :: Int -> [Int] -> [Int]       -- データ長 ソートするリスト 結果リスト
decSort n cs
 | n == 2    = decMergen ([head cs]:[tail cs])  -- 2つのとき リストにしてマージ
 | otherwise = decMergen (xs:ys)                -- 4つ以上のとき 途中で半分に
                 where xs = reverse (incSort n2 (take n2 cs))   -- 前半をソート
                       ys = decMove n2 (drop n2 cs)             -- 後半を分配
                       n2 = n `div` 2
incSort n cs
 | n == 2    = incMergen ([head cs]:[tail cs])
 | otherwise = incMergen (xs:ys)
                 where xs = reverse (decSort n2 (take n2 cs))
                       ys = incMove n2 (drop n2 cs)
                       n2 = n `div` 2

decMove, incMove  :: Int -> [Int] -> [[Int]]    -- スタックに分配する
decMove n cs
 | n == 2    = [head cs]:[tail cs]
 | otherwise = xs:ys
                 where xs = decSort n2 (take n2 cs)     -- 途中で半分に 前半ソート
                       ys = decMove n2 (drop n2 cs)     -- 後半 分配
                       n2 = n `div` 2
incMove n cs
 | n == 2    = [head cs]:[tail cs]
 | otherwise = xs:ys
                 where xs = incSort n2 (take n2 cs)
                       ys = incMove n2 (drop n2 cs)
                       n2 = n `div` 2

cars8 :: [Int]              -- テスト用データ
cars8 = [3,1,4,5,2,6,7,0]
