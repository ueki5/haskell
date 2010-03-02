-- 両替問題(Count Change Problem)

type Amount = Integer
type Coin   = Integer
type Count  = Integer

-- 金額と貨幣(額面)のリストから，両替の場合の数へ

cc :: Amount -> [Coin] -> Count
cc 0 _  = 1                 -- 金額がちょうど0なら，両替は1通り
cc _ [] = 0                 -- 両替に使う貨幣がなければ，両替は0通り
cc a ccs@(c:cs)
 | a < 0     = 0            -- 金額が0より少なければ，両替は0通り
 | otherwise = cc (a-c) ccs -- 最初の種類の貨幣額面を引いた金額の両替の場合の数
             + cc a     cs  -- 最初の種類の貨幣以外を使う両替の場合の数

