{-
����(����)�Υץ����

* ��ꤵ���򤱤� not . not . not . .... �򤢤��ƻȤäƤߤޤ���
  (�ؿ����Ϥ���ʺ��٤ʤ��ȵ��ˤ��ʤ��ΤǤ�:-)
* ���ϥꥹ�Ȥ����Ǥ˽�ʣ�Ϥʤ���Ĺ���� 2^n �Ǥ��뤳�Ȥ��ꤷ�Ƥޤ�
-}

-- inv cmp x y = not (cmp x y)
-- inv cmp = (.) not . cmp
inv = (.) ((.) not)

-- cf. Def of "merge" in hugs/libraries/Data/List.hs
merge :: (Int -> Int -> Bool) -> [Int] -> [Int] -> [Int]
merge cmp xs [] = xs
merge cmp [] ys = ys
merge cmp (x:xs) (y:ys)
  | x `cmp` y  = x : merge cmp xs (y:ys)
  | otherwise  = y : merge cmp (x:xs) ys

mergen :: (Int -> Int -> Bool) -> [[Int]] -> [Int]
mergen cmp = foldl1 (merge cmp)

sort :: (Int -> Int -> Bool) -> [Int] -> [Int]
sort cmp [c] = [c]
sort cmp cs = mergen cmp (xs':ys')
  where xs' = (reverse . sort (inv cmp)) xs
        ys' = move cmp ys
        (xs, ys) = splitAt (length cs `div` 2) cs

move :: (Int -> Int -> Bool) -> [Int] -> [[Int]]
move cmp [c] = [[c]]
move cmp cs = xs':ys'
  where xs' = sort cmp xs
        ys' = move cmp ys
        (xs, ys) = splitAt (length cs `div` 2) cs

incSort, decSort :: [Int] -> [Int]
incSort = sort (<)
decSort = sort (inv (<))

cars8 :: [Int]
cars8 = [3,1,4,5,2,6,7,0]

main = print (incSort cars8)
