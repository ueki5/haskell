{-
岩崎(電通大)のプログラム

私も，4月号の Haskell の連載の第一回を読ませていただきました．
その中の車両のソート問題ですが，すべての関数について dec系とinc系の
二つのバージョンを用意 (同じようなコードが重複している) ところが
気になり，次のようなプログラムを作ってみました．

基本的には，「関係演算」を引数に持たせることにするのですが，
dec の中で inc を呼び，inc の中で dec を呼んだりすることがあるので，
関係演算 (二引数関数) をペアにして持ち歩くことにしました．
関数が First Class ですから，こういうことが簡単にできますね．
(ペアにせず，逆の関係を得たければ，not を合成して関数を作る，という
方法もありますが，再帰のネストが深くなると，not . not . not . ....
と，not が何重にもなってしまうところが気に入りませんでした．)

コードはずいぶん短くなったと思います．
-}

merge2 :: (Int -> Int -> Bool) -> [Int] -> [Int] -> [Int]
merge2 rel [] ys = ys
merge2 rel (x:xs) [] = x:xs
merge2 rel (x:xs) (y:ys) | x `rel` y = x : merge2 rel xs (y:ys)
                         | otherwise = y : merge2 rel (x:xs) ys

mergen :: (Int -> Int -> Bool) -> [[Int]] -> [Int]
mergen rel = foldl1 (merge2 rel)

decMergen = foldl1 (merge2 (>)) --- 動作確認用，本当はいらない
incMergen = foldl1 (merge2 (<)) --- 動作確認用，本当はいらない

sort :: (Int -> Int -> Bool, Int -> Int -> Bool) -> Int -> [Int] -> [Int]
sort (rel1,rel2) n cs
  | n == 2 =    mergen rel1 ([head cs]:[tail cs])
  | otherwise = mergen rel1 (xs:ys)
                where xs = reverse (sort (rel2,rel1) n2 (take n2 cs))
                      ys = move (rel1,rel2) n2 (drop n2 cs)
                      n2 = n `div` 2

decSort, incSort :: Int -> [Int] -> [Int]
decSort = sort ((>),(<))
incSort = sort ((<),(>))

move :: (Int -> Int -> Bool, Int -> Int -> Bool) -> Int -> [Int] -> [[Int]]
move rels n cs | n == 2    = [head cs]:[tail cs]
               | otherwise = xs:ys
                             where xs = sort rels n2 (take n2 cs)
                                   ys = move rels n2 (drop n2 cs)
                                   n2 = n `div` 2

decMove, incMove :: Int -> [Int] -> [[Int]]
decMove = move ((>),(<)) --- 動作確認用，本当はいらない
incMove = move ((<),(>)) --- 動作確認用，本当はいらない

cars8 :: [Int]
cars8 = [3,1,4,5,2,6,7,0]
