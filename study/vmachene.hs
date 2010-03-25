data Expr = Val Int
--          | Mult Expr Expr
          | Add Expr Expr
type Cont = [Op]
data Op = EVAL Expr
--        | MULT Int
        | ADD Int
eval :: Expr -> Cont -> Int
eval (Val n) c = exec c n
eval (Add x y) c = eval x (EVAL y:c)
exec :: Cont -> Int -> Int
exec [] n = n
exec (EVAL y:c) n = eval y (ADD n:c)
exec (ADD n:c) m = exec c (n + m)
value :: Expr -> Int
value e = eval e []
