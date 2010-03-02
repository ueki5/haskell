-- メモ版(memocc)

type Amount = Integer
type Coin   = Integer
type Count  = Integer

memocc :: Amount -> [Coin] -> Table -> (Count, Table)
memocc 0 _  tbl = (1,tbl)                                         --  1:
memocc _ [] tbl = (0,tbl)                                         --  2:
memocc a ccs@(c:cs) tbl                                           --  3:
 | a < 0     = (0,tbl)                                            --  4:
 | otherwise = case lookupTable (a,ccs) tbl of                    --  5: 表を索く
                 (v:_) -> (v,tbl)                                 --  6: 表に登録済の場合
                 []    -> let                                     --  7: 表に未登録の場合
                             (cnt1,tbl1) = memocc (a-c) ccs tbl   --  8: 左の枝
                             (cnt2,tbl2) = memocc a     cs  tbl1  --  9: 右の枝
                             cnt3 = cnt1 + cnt2                   -- 10: 左右の結果の合成
                             tbl3 = insertTable (a,ccs) cnt3 tbl2 -- 11: 表に新たなエントリ追加
                          in (cnt3,tbl3)                          -- 12: 返り値


evalMemoCC :: Amount -> [Coin] -> Count
evalMemoCC amount coins = fst (memocc amount coins emptyTable)

-- Table の定義

type Table = [(Key,Value)]
type Key   = (Amount,[Coin])
type Value = Count

emptyTable :: Table
emptyTable = []

lookupTable :: Key -> Table -> [Value]         -- 表を索く
lookupTable key []          = []
lookupTable key ((k,v):tbl) | key >  k  = []
                            | key == k  = [v]
                            | key <  k  = lookupTable key tbl

insertTable :: Key -> Value -> Table -> Table  -- 表にエントリを追加する
insertTable k v tbl = case break ((k >) . fst) tbl of
                        (xs,ys) -> xs ++ (k,v):ys
