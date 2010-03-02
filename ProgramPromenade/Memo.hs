module Memo where

{- Table -}
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

{- State -}
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

{- ¹â³¬´Ø¿ô memoise -}

type Memo a b = (a -> State (Table a b) b)

memoise :: Ord a => Memo a b -> Memo a b
memoise f x tbl = case lookupTable x tbl of
                    y:_ -> (y,tbl)
                    []  -> let (y,tbl') = f x tbl
                           in  (y,insertTable x y tbl')
