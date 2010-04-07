data Op = Add | Sub | Mul | Div deriving Show
valid :: Op -> Int -> Int -> Bool

-- valid Add x y = x >= y
-- valid Sub x y = x > y
-- valid Mul x y = x /= 1 && y /= 1 && x >= y
-- valid Div x y = y /= 1 && x `mod` y == 0
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
-- show :: Expr -> String
-- show (Val n) = "Val " ++ show n
-- show (App o l r) = "App " ++ "(" ++ show l ++ show r ++ ")"
values :: Expr -> [Int]
values (Val n) = [n]
values (App _ l r) = values l ++ values r
eval :: Expr -> [Int]
eval (Val n) = [n]
eval (App op l r) = [apply op l' r'
                         | l' <- eval l
                         , r' <- eval r
                         , valid op l' r']
subs :: [a] -> [[a]]
subs [] = [[]]
subs (x:xs) = subs xs ++ (map (x:) (subs xs))
interleave :: a -> [a] -> [[a]]
interleave x [] = [[x]]
interleave x (y:ys) = (x:y:ys):(map (y:) (interleave x ys))
-- concat :: [[a]] -> [a]
-- concat [[]] = []
-- concat (x:[]) = x
-- concat (x:xs) = x ++ (concat xs)
perms :: [a] -> [[a]]
perms [] = [[]]
perms (x:xs) = concat (map (interleave x) (perms xs))
choices :: [a] -> [[a]]
choices [] = [[]]
-- choices xs = concat (map perms (subs xs))
choices xs = [xs''| xs' <- subs xs,
                         xs'' <- perms xs']
solution :: Expr -> [Int] -> Int -> Bool
solution e xs n = elem (values e) (choices xs) && (eval e == [n])
split :: [a] -> [([a],[a])]
split [] = []
split [_] = []
split (x:xs) = ([x],xs):[(x:ls,rs)|(ls,rs) <- split xs]
-- split xs = [(take n xs, drop n xs)|n <- [1 .. splitTo]]
--     where splitTo = (length xs) - 1
exprs :: [Int] -> [Expr]
exprs [] = []
exprs [n] = [Val n]
exprs xs = [e | (ls, rs) <- split xs
                       ,l <- exprs ls
                       ,r <- exprs rs
                       ,e <- combine l r]
combine :: Expr -> Expr -> [Expr]
combine l r = [App op l r| op <- ops]
ops :: [Op]
ops = [Add , Sub , Mul , Div]
solutions :: [Int] -> Int -> [Expr]
solutions xs n = [e | xs' <- choices xs,
                      e <- exprs xs',
                      eval e == [n]]
type Result = (Expr, Int)
results :: [Int] -> [Result]
results [] = []
results [n] = [(Val n,n)]
results xs = [r | (ls, rs) <- split xs
                       ,l <- results ls
                       ,r <- results rs
                       ,r <- combine' l r]
combine' :: Result -> Result -> [Result]
combine' (l, x) (r, y) = [(App op l r,apply op x y)| op <- ops, valid op x y]
solutions' :: [Int] -> Int -> [Expr]
solutions' xs n = [e |xs' <- choices xs,
                      (e, n') <- results xs',
                      n' == n]
isChoise' :: Ord a => a -> [a] -> [a]
isChoise' x [] = []
isChoise' x (y:ys) | x == y = ys
                   | otherwise = y:(isChoise' x ys)
isChoise :: Ord a => [a] -> [a] -> Bool
isChoise xs (y:ys) = isChoise (isChoise' y xs) ys
isChoise xs ys | xs == [] = True
               | xs /= [] && ys == [] = False
allCase :: [Int] -> [Expr]
-- allCase xs = concat (map exprs (choices xs))
allCase xs = [es | xs' <- choices xs,
                   es    <- exprs xs']
validCase :: [Int] -> [Expr]
validCase xs = [es | es <- allCase xs, eval es /= []]
-- data CheckVal = Infinate | Int
-- solutions''' :: [Int] -> Int -> ChackVal -> ([Expr], [Expr])
-- solutions''' xs n = [e |xs' <- choices xs,
--                       (e, n') <- results xs',
--                       n' == n]
v1 = Val 1
v2 = Val 2
e1 = App Add v1 v2
e2 = App Mul v1 v2
e3 = App Div v2 v1
e4 = App Sub v2 v1
e5 = App Div v1 v2
e6 = App Sub v1 v2
e7 = App Mul e1 e2
main = do putStrLn ("1 + 2 = " ++ show (eval e1))
          putStrLn ("1 * 2 = " ++ show (eval e2))
          putStrLn ("2 / 1 = " ++ show (eval e3))
          putStrLn ("2 - 1 = " ++ show (eval e4))
          putStrLn ("1 - 2 = " ++ show (eval e5))
          putStrLn ("1 / 2 = " ++ show (eval e6))
          putStrLn ("(1 + 2) * (1 * 2) = " ++ show (eval e7))
