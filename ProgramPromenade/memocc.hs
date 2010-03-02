-- �����(memocc)

type Amount = Integer
type Coin   = Integer
type Count  = Integer

memocc :: Amount -> [Coin] -> Table -> (Count, Table)
memocc 0 _  tbl = (1,tbl)                                         --  1:
memocc _ [] tbl = (0,tbl)                                         --  2:
memocc a ccs@(c:cs) tbl                                           --  3:
 | a < 0     = (0,tbl)                                            --  4:
 | otherwise = case lookupTable (a,ccs) tbl of                    --  5: ɽ�����
                 (v:_) -> (v,tbl)                                 --  6: ɽ����Ͽ�Ѥξ��
                 []    -> let                                     --  7: ɽ��̤��Ͽ�ξ��
                             (cnt1,tbl1) = memocc (a-c) ccs tbl   --  8: ���λ�
                             (cnt2,tbl2) = memocc a     cs  tbl1  --  9: ���λ�
                             cnt3 = cnt1 + cnt2                   -- 10: �����η�̤ι���
                             tbl3 = insertTable (a,ccs) cnt3 tbl2 -- 11: ɽ�˿����ʥ���ȥ��ɲ�
                          in (cnt3,tbl3)                          -- 12: �֤���


evalMemoCC :: Amount -> [Coin] -> Count
evalMemoCC amount coins = fst (memocc amount coins emptyTable)

-- Table �����

type Table = [(Key,Value)]
type Key   = (Amount,[Coin])
type Value = Count

emptyTable :: Table
emptyTable = []

lookupTable :: Key -> Table -> [Value]         -- ɽ�����
lookupTable key []          = []
lookupTable key ((k,v):tbl) | key >  k  = []
                            | key == k  = [v]
                            | key <  k  = lookupTable key tbl

insertTable :: Key -> Value -> Table -> Table  -- ɽ�˥���ȥ���ɲä���
insertTable k v tbl = case break ((k >) . fst) tbl of
                        (xs,ys) -> xs ++ (k,v):ys
