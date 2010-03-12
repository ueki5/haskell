--type String = [Char]
type Pos = (Int, Int)
type Board = [Pos]
type Parser a = String -> [(a, String)]
--type IO a = World -> (a, World)
type Assoc k v = [(k, v)]
find::Eq k => k -> Assoc k v -> v
find k assoc = head [v|(k', v) <- assoc,k == k']
data Move = Hidari|Migi|Ue|Sita
move::Move -> IO ()
move Hidari = do putStrLn "Left"
move Migi = do putStrLn "Right"
move Ue = do putStrLn "Up"
move Sita = do putStrLn "Down"
gyaku::Move -> Move
gyaku Hidari = Migi
gyaku Migi = Hidari
gyaku Ue = Sita
gyaku Sita = Ue
data Shape = Circle Float|Rect Float Float
square::Float -> Shape
square n = Rect n n
area::Shape -> Float
area (Circle r) = pi * r^2
area (Rect x y) = x * y
--data Kamo a = Nothing | Just a
intKamo::Int -> Maybe Int
intKamo 0 = Just 0
intKamo _ = Nothing
safediv::Int -> Int -> Maybe Int
safediv _ 0 = Nothing
safediv m n = Just (m `div` n)
data Nat = Zero|Succ Nat
-- nat2str::Char -> Nat -> [Char]
-- nat2str _ Zero = []
-- nat2str c (Succ n) = (c:nat2str n)
-- str2nat::Char -> [Char] -> Nat
-- str2nat _ [] = Zero
-- str2nat c (x:xs) | c == x = Succ (str2nat c xs)
--                  | otherwise = Zero
-- nat2str::Nat -> [Char]
-- nat2str Zero = []
-- nat2str (Succ n) = '0':(nat2str n)
str2nat::[Char] -> Nat
str2nat [] = Zero
str2nat (x:xs) = Succ (str2nat xs)
instance Show Nat where
    show Zero = ""
    show (Succ n) = "0" ++ show n
data List a = Nil | Cons a (List a)
len::List a -> Int
len Nil = 0
len (Cons _ xs) = 1 + len xs
data Tree = Leaf Int | Node Tree Int Tree
occurs::Int -> Tree -> Bool
occurs m (Leaf n) = m == n
occurs m (Node l n r) | m < n = occurs m l
                      | m >= n = occurs m r
