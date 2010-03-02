{- サイコロ置換 : dice.hs -}

import List                 -- Listのモジュールを読み込む

data Obj = T | S | E | B | N | W deriving (Eq, Enum, Show)
type Perm = [(Obj, Obj)]    -- 型に名前をつける
type Cycle = [Obj]
type CyclePerm = [Cycle]

allObj :: [Obj]
allObj = [T .. W]           -- Objの全体 (リスト)

go :: Obj -> Cycle -> Obj   -- goesToの下請け 1個のサイクルでの移動
go o c = case elemIndices o c of
         []  -> o
         [i] -> cycle c !! succ i   -- cycle c は c++c++...,succ i は i + 1

goesTo :: Obj -> CyclePerm -> Obj
goesTo = foldl go           -- Obj のリストに左から go を作用させる

assoc :: Eq k => k -> [(k, v)] -> [(k, v)]      -- Lispのassocのようなもの
assoc c as = [(k, v) | (k, v) <- as, c == k]

makeCycle0 :: Perm -> CyclePerm -> CyclePerm    -- サイクル表現にする
makeCycle0 [] qs = qs
makeCycle0 ((x,y):ss) qs
  | x == y   = makeCycle0 ss qs                 -- 単一サイクル除去
  |otherwise = makeCycle1 ss ([x,y]:qs)

makeCycle1 :: Perm -> CyclePerm -> CyclePerm
makeCycle1 ss (cs:css) 
  | c == head cs  = makeCycle0 ss' (cs:css)
  | otherwise     = makeCycle1 ss' ((cs ++ [c]):css)
        where c = snd d
              d = head (assoc (last cs) ss)
              ss' = delete d ss

prodPerm :: [CyclePerm] -> CyclePerm       -- 各種の手順による置換の計算
prodPerm ops = makeCycle0 (zip allObj allObj') []
            where allObj' = map (flip goesTo (concat ops)) allObj

t,s,e,b,n,w :: CyclePerm    -- 回転の定義
t = [[S,W,N,E]]
s = [[E,B,W,T]]
e = [[B,S,T,N]]
b = [[N,W,S,E]]
n = [[W,B,E,T]]
w = [[T,S,B,N]]
