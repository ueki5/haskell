{- ���������ִ� : dice.hs -}

import List                 -- List�Υ⥸�塼����ɤ߹���

data Obj = T | S | E | B | N | W deriving (Eq, Enum, Show)
type Perm = [(Obj, Obj)]    -- ����̾����Ĥ���
type Cycle = [Obj]
type CyclePerm = [Cycle]

allObj :: [Obj]
allObj = [T .. W]           -- Obj������ (�ꥹ��)

go :: Obj -> Cycle -> Obj   -- goesTo�β����� 1�ĤΥ�������Ǥΰ�ư
go o c = case elemIndices o c of
         []  -> o
         [i] -> cycle c !! succ i   -- cycle c �� c++c++...,succ i �� i + 1

goesTo :: Obj -> CyclePerm -> Obj
goesTo = foldl go           -- Obj �Υꥹ�Ȥ˺����� go ����Ѥ�����

assoc :: Eq k => k -> [(k, v)] -> [(k, v)]      -- Lisp��assoc�Τ褦�ʤ��
assoc c as = [(k, v) | (k, v) <- as, c == k]

makeCycle0 :: Perm -> CyclePerm -> CyclePerm    -- ��������ɽ���ˤ���
makeCycle0 [] qs = qs
makeCycle0 ((x,y):ss) qs
  | x == y   = makeCycle0 ss qs                 -- ñ�쥵���������
  |otherwise = makeCycle1 ss ([x,y]:qs)

makeCycle1 :: Perm -> CyclePerm -> CyclePerm
makeCycle1 ss (cs:css) 
  | c == head cs  = makeCycle0 ss' (cs:css)
  | otherwise     = makeCycle1 ss' ((cs ++ [c]):css)
        where c = snd d
              d = head (assoc (last cs) ss)
              ss' = delete d ss

prodPerm :: [CyclePerm] -> CyclePerm       -- �Ƽ�μ��ˤ���ִ��η׻�
prodPerm ops = makeCycle0 (zip allObj allObj') []
            where allObj' = map (flip goesTo (concat ops)) allObj

t,s,e,b,n,w :: CyclePerm    -- ��ž�����
t = [[S,W,N,E]]
s = [[E,B,W,T]]
e = [[B,S,T,N]]
b = [[N,W,S,E]]
n = [[W,B,E,T]]
w = [[T,S,B,N]]
