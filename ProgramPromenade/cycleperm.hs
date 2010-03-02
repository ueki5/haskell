{- KnuthのAlgorithm B : cycleperm.hs -}

{- see page 173 of The Art of Computer Programming, Vol.1 3rd ed. -}
{- Algorithm B (Multiply permutations in cycle form) -}

import List

perms :: [String]
perms = ["acfg","bcd","aed","fade","bgfae"]

goesTo :: [String] -> (String, String)
goesTo ss = (allchar, allgo)
  where allchar = sort (nub (foldl1 (++) ss))
        allgo = fst (foldr go (allchar, '?') (map reverse ss))
        go [] (ps, o) = (a ++ (o: tail b), o')
          where [i] = elemIndices '?' ps
                (a,b) = splitAt i ps
                o' = head b
        go (c:cs) (ps, o) = go cs (ps', o')
          where [i] = elemIndices c allchar
                (a,b) = splitAt i ps
                ps' = a ++ (o: tail b)
                o' = head b
{- Table 2
     ( a c f g ) ( b c d ) ( a e d ) ( f a d e ) ( b g f a c )
a ->|d d a a a a|a a a a a|a a d d d|d d d e e e|e e e e e a a
b ->|c c c c c c|c c g g g|g g g g g|g g g g g g|g g b b b b b
c ->|e e e d d d|d d d c c|c c c c c|c c c c c c|c c c c c c c
d ->|g g g g g g|g ? ? ? d|d ? ? ? b|b b b b d d|d d d d d d d
e ->|b b b b b b|b b b b b|b b b a a|a ? ? ? ? b|b ? ? ? ? ? e
f ->|f f f f e e|e e e e e|e e e e e|e e a a a a|a a a a f f f
g ->|a ? ? ? ? f|f f f f f|f f f f f|f f f f f f|f f f g g g g

Main> goesTo perms
("abcdefg","dcegbfa")

goを外に出して, reverseしたものを使い, 一歩ずつ実行する

go :: String -> (String, Char) -> (String, Char)
go [] (ps, o) = (a ++ (o: tail b), o')
  where [i] = elemIndices '?' ps
        (a,b) = splitAt i ps
        o' = head b
go (c:cs) (ps, o) = go cs (ps', o')
  where [i] = elemIndices c "abcdefg"
        (a,b) = splitAt i ps
        ps' = a++(o:tail b)
        o' = head b

Main> reverse (map reverse perms)
["eafgb","edaf","dea","dcb","gfca"]

Main> go "eafgb" ("abcdefg", '?')
("egcdbaf",'?')
Main> go "edaf" ("egcdbaf",'?')
("dgcbaef",'?')
Main> go "dea" ("dgcbaef",'?')
("agcdbef",'?')
Main> go "dcb" ("agcdbef",'?')
("acdgbef",'?')
Main> go "gfca" ("acdgbef",'?')
("dcegbfa",'?')
-}
