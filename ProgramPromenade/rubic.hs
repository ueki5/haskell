{- ルービックキューブ : rubic.hs -}

import List

-- 2面体と3面体の型
data Obj = TE | TS | TW | TN | ET | ES | EB | EN | ST | SE | SB | SW
         | BE | BS | BW | BN | WT | WS | WB | WN | NT | NE | NB | NW
         | TES | EST | STE | TSW | SWT | WTS | TWN | WNT | NTW | TNE
         | NET | ETN | BSE | SEB | EBS | BEN | ENB | NBE | BNW | NWB
         | WBN | BWS | WSB | SBW deriving (Eq, Show)

allObj :: [Obj]         -- すべての2面体と3面体のリスト
allObj = [TE, TS, TW, TN, ET, ES, EB, EN, ST, SE, SB, SW, 
          BE, BS, BW, BN, WT, WS, WB, WN, NT, NE, NB, NW,
          TES, EST, STE, TSW, SWT, WTS, TWN, WNT, NTW, TNE,
          NET, ETN, BSE, SEB, EBS, BEN, ENB, NBE, BNW, NWB,
          WBN, BWS, WSB, SBW]

goesTo :: Obj -> [[Obj]] -> Obj
goesTo c [] = c
goesTo c (s:ss) | 
  | length x == 0     = goesTo c ss
  | y == length s - 1 = goesTo (s!!0) ss
  | otherwise         = goesTo (s!!(y + 1)) ss
                          where [y] = x
                                x = elemIndices c s

assoc :: Obj -> [(Obj,Obj)] -> (Obj,Obj)
assoc c ((x,y):ss)
  | c == x    = (x,y)
  | otherwise = assoc c ss

-- makeCycle multiplies permutations in cycle form   置換の積を計算

makeCycle0 :: [(Obj,Obj)] -> [[Obj]] -> [[Obj]]
makeCycle0 [] qs = qs
makeCycle0 ((x,y):ss) qs
  | x == y   = makeCycle0 ss qs
  |otherwise = makeCycle1 ss ([x,y]:qs)

makeCycle1 :: [(Obj,Obj)] -> [[Obj]] -> [[Obj]]
makeCycle1 ss (cs:css) 
  | c == head cs  = makeCycle0 ss' (cs:css)
  | otherwise     = makeCycle1 ss' ((cs ++ [c]):css)
        where c = snd d
              d = assoc (last cs) ss
              ss' = delete d ss

prodPerm :: [[[Obj]]] -> [[Obj]]       -- 各種の手順による置換の計算
prodPerm ops = makeCycle0 (zip allObj allGo) []
            where allGo = map (\x -> goesTo x (concat ops)) allObj

e,e',s,s',w,w',n,n',t,t',b,b' :: [[Obj]] -- e(東)などの面を時計回りに90度回転
e = [[SE,TE,NE,BE],[ES,ET,EN,EB],
     [TES,NET,BEN,SEB],[STE,TNE,NBE,BSE],[EST,ETN,ENB,EBS]]
s = [[WS,TS,ES,BS],[SW,ST,SE,SB],
     [TSW,EST,BSE,WSB],[WTS,TES,EBS,BWS],[SWT,STE,SEB,SBW]]
w = [[NW,TW,SW,BW],[WN,WT,WS,WB],
     [TWN,SWT,BWS,NWB],[NTW,TSW,SBW,BNW],[WNT,WTS,WSB,WBN]]
n = [[EN,TN,WN,BN],[NE,NT,NW,NB],
     [TNE,WNT,BNW,ENB],[ETN,TWN,WBN,BEN],[NET,NTW,NWB,NBE]]
t = [[ST,WT,NT,ET],[TS,TW,TN,TE],
     [WTS,NTW,ETN,STE],[SWT,WNT,NET,EST],[TSW,TWN,TNE,TES]]
b = [[SB,EB,NB,WB],[BS,BE,BN,BW],
     [EBS,NBE,WBN,SBW],[SEB,ENB,NWB,WSB],[BSE,BEN,BNW,BWS]]
e' = prodPerm [e,e,e]  -- e'などはeなどの反時計回りの回転
s' = prodPerm [s,s,s]
w' = prodPerm [w,w,w]
n' = prodPerm [n,n,n]
t' = prodPerm [t,t,t]
b' = prodPerm [b,b,b]
