data Op = Add | Sub | Mul | Div
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
data Expr = Val Int | App Op Expr Expr
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
choices xs = concat (map perms (subs xs))
solution :: Expr -> [Int] -> Int -> Bool
solution e xs x = elem (values e) (choices xs) && (eval e == [x])
split :: [a] -> [([a],[a])]
split [] = []
split [_] = []
split (x:xs) = ([x],xs):[(x:ls,rs)|(ls,rs) <- split xs]
-- split xs = [(take n xs, drop n xs)|n <- [1 .. splitTo]]
--     where splitTo = (length xs) - 1
exprs :: [Int] -> [Expr]
exprs [] -> []
exprs [n] -> Val n
exprs 
-- combine :: Expr -> Expr -> [Expr]
-- ops :: [Op]
-- solutions :: [Int] -> Int -> [Expr]
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
