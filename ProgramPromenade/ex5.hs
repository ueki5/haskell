import List
import Ticket hiding (trees,same,ticket,allterms)

-- 練習問題 5 の解答例

trees :: [Char] -> [Char] -> [(Rat,Term)]
trees ds os = [ t | (_,t) <- vtrees ds os ]

vtrees :: [Char] -> [Char] -> [([Char],(Rat,Term))]
vtrees [c] os = [(os, (ctor c, Val c))]
vtrees ds  os = concat [ odtree os xs ys | (xs,ys) <- splits1 ds ]

odtree :: [Char] -> [Char] -> [Char] -> [([Char],(Rat,Term))]
odtree os ls rs 
      = [ (os'', (ctoo o u v, App o l r)) | (o:os',(u,l)) <- vtrees ls os
                                          , (os'' ,(v,r)) <- vtrees rs os' ]

same :: Int -> ((Rat,Term) -> Bool)
same i ((n,d),_) = i*d == n && d /= 0

ticket :: Int -> [Char] -> Term
ticket n ds = snd $ head (filter (same n) (allterms ds))

allterms :: [Char] -> [(Rat,Term)]
allterms ds = concat [ trees ns os | ns <- perm ds, os <- rperm ops4 (length ds - 1) ]