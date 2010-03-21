data Nat = Zero | Succ Nat
nat2int :: Nat -> Int
nat2int Zero = 0
nat2int (Succ n) = 1 + nat2int n
add :: Nat -> Nat -> Nat
add Zero n = n
add (Succ n) m = Succ (add n m)
--add n m = add m n
mult :: Nat -> Nat -> Nat
mult Zero n = Zero
mult (Succ n) m = add m (mult n m)

one = Succ Zero
two = Succ one
three = Succ two
four = Succ three
five = Succ four
six = Succ five
seven = Succ six
eight = Succ seven
nine = Succ eight
ten = Succ nine

type Decimal = [Nat]
eval :: Decimal -> Int
-- eval d = eval' Zero d
--               where 
--                 eval' :: Nat -> Decimal -> Nat
--                 eval' n [] = n
--                 eval' n (x:xs) = eval' (add (mult ten n) x) xs
-- eval d = nat2int (foldr (\a b -> add  a (mult ten b)) Zero d)
eval d = nat2int (foldl (\a b -> add  (mult ten a) b) Zero d)

