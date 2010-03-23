-- data List a = Nil | Cons a (List a) deriving (Show,Read)
-- data List a = Nil | Cons a (List a)
data Show a => List a = Nil | Cons a (List a)
instance Show a => Show (List a) where
    show Nil = "Nil"
    show (Cons a ax)  = "Cons" ++ " " ++ show a ++ " (" ++ show ax ++ ")"
len :: Show a => List a -> Int
len Nil = 0
len (Cons x xs) = 1 + (len xs)
cons :: Show a => a -> List a -> List a
cons a ax = Cons a ax
a = cons 1 Nil
a' = cons 1 a
a'' = cons 1 a'
first :: Show a => List a -> Maybe a
first Nil = Nothing
first (Cons a ax) = Just a
rest :: Show a => List a -> Maybe (List a)
rest Nil = Nothing
rest (Cons a Nil) = Nothing
rest (Cons a ax) = Just ax


-- -- ’²¸ start
-- data Show a => Ueki a = Ueki a
-- instance Show a => Show (Ueki a) where
--     show (Ueki a) = "Ueki " ++ (show a)
-- k = Ueki 1
-- -- end

-- Tree
data {-Num a =>-} Tree a = Leaf a |Node (Tree a) (Tree a)
occurs :: Num a => a -> Tree a -> Bool
occurs a (Leaf a') = (==) a a'
occurs a (Node first second) = occurs a first || occurs a second
t1 = Leaf 1
t2 = Leaf 2
t3 = Node t1 t2
-- unfold :: (a -> (a, b)) -> (a -> Bool) -> a -> [b]
-- unfold fun fil init = case of 