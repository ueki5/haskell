module Ch18.UWriter where
main = 
--   do
--   let a = 1 + 2
--       b = a * 3
--       c = b - 4
--   print $ show a ++ "," ++ show b ++ "," ++ show c ++ ","
--   do
--   let (a,loga) = out (1 + 2)
--       (b,logb) = out (a * 3)
--       (c,logc) = out (b - 4)
--   print $ loga ++ "," ++ logb ++ "," ++ logc ++ ","
  let (x, log) = out (1 + 2) `comb` \a -> 
                 out (a * 3) `comb` \b -> 
                 out (b - 4)
  in log
                           
type Log = (Int, String)
out :: Int -> Log
out n = (n, show n ++ ",")
comb m n = let (a, log1) = m
               (b, log2) = n a
           in  (b, log1 ++ log2)
