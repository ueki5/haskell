{- Table をキーと対応する値でパラメータ化する -}

type Table k v = [(k,v)]

emptyTable :: Table a b
lookupTable :: Ord k => k -> Table k v -> [v]
insertTable :: Ord k => k -> v -> Table k v -> Table k v

emptyTable = []

lookupTable key []          = []
lookupTable key ((k,v):tbl) | key >  k  = []
                            | key == k  = [v]
                            | key <  k  = lookupTable key tbl

insertTable k v tbl = case break ((k >) . fst) tbl of
                        (xs,ys) -> xs ++ (k,v):ys

{-
memocc :: (Amount,[Coins]) -> Table Amount Count -> (Count,Table Amount Count)
memocc (0,_ ) tbl = (1,tbl)
memocc (_,[]) tbl = (0,tbl)
memocc arg@(a,ccs@(c:cs)) tbl
 | a < 0     = (0,tbl)
 | otherwise = case lookupTable arg tbl of
                 (v:_) -> (v,tbl)
                 []    -> let
                             (cnt1,tbl1) = memocc (a-c,ccs) tbl
                             (cnt2,tbl2) = memocc (a  , cs) tbl1
                             cnt3 = cnt1 + cnt2
                             tbl3 = insertTable arg cnt3 tbl2
                          in (cnt3,tbl3)

evalMemoCC :: Amount -> [Coin] -> Count
evalMemoCC amount coins = fst (memocc (amount,coins) emptyTable)
-}

{-
関心のあるデータ(\texttt{t}型)とは別に，変化する状態(\texttt{s}型)を
次へと伝えていく仕組み
-}

type State s t = s -> (t,s)

withState :: t -> State s t
withState x = \ state -> (x,state)

bindState :: State s t -> (t -> State s u) -> State s u
bindState sx sf s0 = let (x,s1) = sx s0 in sf x s1

evalState :: State s t -> s -> t
evalState s s0 = fst (s s0)

fun1WithState :: (a -> b) -> State s a -> State s b
fun1WithState f sx = bindState sx (\ x -> withState (f x))

fun2WithState :: (a -> b -> c) -> State s a -> State s b -> State s c
fun2WithState f sx sy = bindState sx (\ x -> 
                        bindState sy (\ y -> 
                        withState (f x y)))

{- 高階関数 memoise -}

memoise :: Ord a => (a -> State (Table a b) b) -> a -> State (Table a b) b
memoise f x tbl = case lookupTable x tbl of
                    y:_ -> (y,tbl)
                    []  -> let (y,tbl') = f x tbl
                           in  (y,insertTable x y tbl')

{-
memocc (0,_ ) = withState 1
memocc (_,[]) = withState 0
memocc arg@(a,_)
 | a < 0     = withState 0
 | otherwise = memoise (\ (a,ccs@(c:cs)) -> memocc (a-c,ccs) `add` memocc (a,cs)) arg
  where add = fun2WithState (+)
-}

instance Num b => Eq (State (Table a b) b) where
  sx == sy    = (evalState sx emptyTable) == (evalState sy emptyTable)

instance Num b => Show (State (Table a b) b) where
  show sx     = show (evalState sx emptyTable)

instance Num b => Num (State (Table a b) b) where
  (+)         = fun2WithState (+)
  (-)         = fun2WithState (-)
  (*)         = fun2WithState (*)
  negate      = fun1WithState negate
  abs         = fun1WithState abs
  signum      = fun1WithState signum
  fromInteger = withState . fromInteger

type Amount = Integer
type Coin   = Integer
type Count  = Integer

type Memo a b = (a -> State (Table a b) b)

memocc :: Memo (Amount,[Coin]) Count
memocc (0,_ ) = 1
memocc (_,[]) = 0
memocc arg@(a,_)
 | a < 0      = 0
 | otherwise  = memoise (\ (a,ccs@(c:cs)) -> memocc (a-c,ccs) + memocc (a,cs)) arg

evalMemoCC :: Amount -> [Coin] -> Count
evalMemoCC amount coins = evalState (memocc (amount,coins)) emptyTable


