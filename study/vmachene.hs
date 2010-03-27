data Expr = Val Int
          | Add Expr Expr
          | Mlt Expr Expr
type Cont = [Op]
data Op = EVAL Expr
        | ADDOP
        | ADD Int
        | MLTOP
        | MLT Int
eval :: Expr -> Cont -> Int
eval (Val n) c = exec c n
eval (Add x y) c = eval x (EVAL y:ADDOP:c)
eval (Mlt x y) c = eval x (EVAL y:MLTOP:c)
-- eval (Add x y) c = eval y (EVAL x:ADDOP:c)
-- eval (Mlt x y) c = eval y (EVAL x:MLTOP:c)
exec :: Cont -> Int -> Int
exec [] n = n
exec (EVAL e:ADDOP:c) n = eval e (ADD n:c)
exec (EVAL e:MLTOP:c) n = eval e (MLT n:c)
exec (ADD n:c) m = exec c (n + m)
exec (MLT n:c) m = exec c (n * m)
value :: Expr -> Int
value e = eval e []
t1  = Add v1 v2
t2  = Add v1 v3
t3  = Add v2 v1
t4  = Mlt v1 v2
t5  = Mlt v1 v3
t6  = Mlt v2 v1
t7  = Mlt (Add v1 v2) (Add v3 v4)
t8  = Add (Mlt v1 v2) (Mlt v3 v4)
t9  = Add v1 (Mlt v2 (Add v3 v4))
t10 = Mlt v1 (Add v2 (Mlt v3 v4))
main = do putStrLn ("1 + 2 = " ++ show (value v1))
          putStrLn ("1 + 3 = " ++ show (value v2))
          putStrLn ("2 + 1 = " ++ show (value v3))
          putStrLn ("1 * 2 = " ++ show (value t4))
          putStrLn ("1 * 3 = " ++ show (value t5))
          putStrLn ("2 * 1 = " ++ show (value t6))
          putStrLn ("(1 + 2) * (3 + 4) = " ++ show (value t7))
          putStrLn ("(1 * 2) + (3 * 4) = " ++ show (value t8))
          putStrLn ("1 + (2 * (3 + 4)) = " ++ show (value t9))
          putStrLn ("1 * (2 + (3 * 4)) = " ++ show (value t10))
v1  = Val 1
v2  = Val 2
v3  = Val 3
v4  = Val 4
v5  = Val 5
v6  = Val 6
v7  = Val 7
v8  = Val 8
v9  = Val 9
-- value t9
-- eval t9 []
-- eval v1 (EVAL (Mlt v2 (Add v3 v4)):ADDOP:[])
-- exec (EVAL (Mlt v2 (Add v3 v4)):ADDOP:[]) 1
-- eval (Mlt v2 (Add v3 v4)) [ADD 1]
-- eval v2 (EVAL (Add v3 v4)):MLTOP:(ADD 1)
-- exec (EVAL (Add v3 v4)):MLTOP:(ADD 1) 2
-- eval (Add v3 v4) (MLT 2):(ADD 1)
-- eval v3 (EVAL v4):ADDOP:(MLT 2):(ADD 1)
-- exec (EVAL v4):ADDOP:(MLT 2):(ADD 1) 3
