data Op = Add | Sub | Mul | Div deriving Show
valid :: Op -> Int -> Int -> Bool
valid Add _ _ = True
valid Sub x y = x > y
valid Mul _ _ = True
valid Div x y = x `mod` y == 0
apply :: Op -> Int -> Int -> Int
apply Add x y = x + y
apply Sub x y = x - y
apply Mul x y = x * y
apply Div x y = x `div` y
data Expr = Val Int | App Op Expr Expr deriving Show
values :: Expr -> [Int]
values (Val n) = [n]
values (App _ l r) = values l ++ values r
eval :: Expr -> [Int]
eval (Val n) = [n]
eval (App op l r) = [apply op l' r'| l' <- eval l, r' <- eval r, valid op l' r']
subs :: [a] -> [[a]]
subs [] = [[]]
subs (x:xs) = subs xs ++ (map (x:) (subs xs))
interleave :: a -> [a] -> [[a]]
interleave x [] = [[x]]
interleave x (y:ys) = (x:y:ys):(map (y:) (interleave x ys))
perms :: [a] -> [[a]]
perms [] = [[]]
perms (x:xs) = concat (map (interleave x) (perms xs))
choices :: [a] -> [[a]]
choices [] = [[]]
choices xs = [xs''| xs' <- subs xs, xs'' <- perms xs']
solution :: Expr -> [Int] -> Int -> Bool
solution e xs n = elem (values e) (choices xs) && (eval e == [n])
split :: [a] -> [([a],[a])]
split [] = []
split [_] = []
split (x:xs) = ([x],xs):[(x:ls,rs)|(ls,rs) <- split xs]
exprs :: [Int] -> [Expr]
exprs [] = []
exprs [n] = [Val n]
exprs xs = [e | (ls, rs) <- split xs, l <- exprs ls, r <- exprs rs, e <- combine l r]
combine :: Expr -> Expr -> [Expr]
combine l r = [App op l r| op <- ops]
ops :: [Op]
ops = [Add , Sub , Mul , Div]
solutions :: [Int] -> Int -> [Expr]
solutions xs n = [e | xs' <- choices xs, e <- exprs xs', eval e == [n]]
isChoise' :: Ord a => a -> [a] -> [a]
isChoise' x [] = []
isChoise' x (y:ys) | x == y = ys
                   | otherwise = y:(isChoise' x ys)
isChoise :: Ord a => [a] -> [a] -> Bool
isChoise xs (y:ys) = isChoise (isChoise' y xs) ys
isChoise xs ys | xs == [] = True
               | xs /= [] && ys == [] = False
allCase :: [Int] -> [Expr]
allCase xs = [es | xs' <- choices xs, es <- exprs xs']
validCase :: [Int] -> [Expr]
validCase xs = [es | es <- allCase xs, eval es /= []]
sortByFunc :: (Expr -> Expr -> Bool) -> [Expr] -> [Expr]
sortByFunc f [] = []
sortByFunc f (e:es) = (sortByFunc f les) ++ [e] ++ (sortByFunc f res)
    where 
      les = [le | le <- es, f e le == True]
      res = [re | re <- es, f e re /= True]
compByValue :: Int -> Expr -> Expr -> Bool
compByValue n le re = abs (lv - n) > abs (rv - n)
    where
      [lv] = eval le
      [rv] = eval re
sortByValue :: Int -> [Expr] -> [Expr]
sortByValue n es = sortByFunc (compByValue n) es
main = do print ("e1=" ++ show e1)
          print ("v1=" ++ show (eval e1))
          print ("e2=" ++ show e2)
          print ("v2=" ++ show (eval e2))
    where ptn1 = validCase [1,3,7,10,25,50]
          e1 = head (sortByValue 765 ptn1)
          e2 = head (sortByValue 831 ptn1)
