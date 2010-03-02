import List
import Ticket hiding (trees)

-- 練習問題 4 の解答例

trees :: [Char] -> [Char] -> [Term]
trees ds os = [ t | (_,t) <- trees' ds os ]

trees' :: [Char] -> [Char] -> [([Char],Term)]
trees' [c] os = [(os, Val c)]
trees' ds  os = concat [ odtree os xs ys | (xs,ys) <- splits1 ds ]

odtree :: [Char] -> [Char] -> [Char] -> [([Char],Term)]
odtree os ls rs 
 = [ (os'', App o l r) | (o:os',l) <- trees' ls os, (os'',r) <- trees' rs os' ]
