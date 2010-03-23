data Prop = Const Bool
          | Var Char
          | Not Prop
          | And Prop Prop
          | Or Prop Prop
          | Imply Prop Prop
          | Equal Prop Prop
p1 :: Prop
p1 = And (Var 'A') (Not (Var 'A'))
p2 :: Prop
p2 = Imply (And (Var 'A')(Var 'B')) (Var 'A')
p3 :: Prop
p3 = Imply (Var 'A') (And (Var 'A') (Var 'B'))
p4 :: Prop
p4 = Imply (And (Var 'A') (Imply (Var 'A') (Var 'B'))) (Var 'B')
p5 = Imply (Not (Or (Var 'A') (Var 'B'))) (And (Not (Var 'A')) (Not (Var 'B')))
p6 = Equal (Not (Not (Var 'A'))) (Var 'A')
type Subst = Assoc Char Bool
type Assoc k v =  [(k,v)]
find :: Eq k => k -> Assoc k v -> v
find k t =  head [v | (k',v) <- t, k == k']
eval :: Subst -> Prop -> Bool
eval _ (Const b) = b
eval s (Var k) = find k s
eval s (Not p) = not (eval s p)
eval s (And p p') = eval s p && eval s p'
eval s (Or p p') = eval s p || eval s p'
eval s (Imply p p') = eval s p <= eval s p'
eval s (Equal p p') = eval s p == eval s p'
vars :: Prop -> [Char]
vars (Const b) = []
vars (Var c) = [c]
vars (Not p) = vars p
vars (And p p') = vars p ++ vars p'
vars (Or p p') = vars p ++ vars p'
vars (Imply p p') = vars p ++ vars p'
vars (Equal p p') = vars p ++ vars p'
bools :: Int -> [[Bool]]
bools 0 = [[]]
bools (n + 1) = map (False:) (bools n) ++ map (True:) (bools n)
rmdups :: Eq a => [a] -> [a]
rmdups [] = []
rmdups (x:xs) = x:(filter (x /=) (rmdups xs))
substs :: Prop -> [Subst]
substs p = map (zip vs) (bools (length vs))
           where vs = rmdups (vars p)
isTaut :: Prop -> Bool
isTaut p = and [eval s p | s <- substs p]
