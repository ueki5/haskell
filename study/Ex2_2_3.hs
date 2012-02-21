module Pcc.Ex2_2_3 where
input = "ABCDEFGAAAAA"

solve :: [Char] -> ([Char],[Char])
solve is = solve' (is,[])
solve' :: ([Char],[Char]) -> ([Char],[Char])
solve' ([],os) = ([],os)
solve' (is,os) = case is > reverse is of
  True -> solve' (tail $ reverse is, os ++ [(head $ reverse is)])
  False -> solve' (tail $ is,os ++ [head is])
                               
