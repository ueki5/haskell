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
data {-Ord a =>-} Tree a = Leaf a |Node (Tree a) (Tree a) deriving (Show)
occurs :: Ord a => a -> Tree a -> Bool
occurs a (Leaf a') = (==) a a'
occurs a (Node fst snd) = occurs a fst || occurs a snd
t1 = Leaf 1
t2 = Leaf 2
t3 = Node t1 t2
t1' = Leaf 1
t1'' = Leaf 1
t4 = Node t1' t3
t5 = Node t1'' t4
count :: Tree a -> Int
count (Leaf a) = 1
count (Node a a') = (count a) + (count a')
balanced :: Tree a -> Bool
balanced (Leaf _) = True
balanced (Node a a') = if check left right
                       then (balanced a) && (balanced a')
                       else False
                         where 
                           check n m = if abs (n - m) <= 1
                                           then True 
                                           else False
                           left = count a
                           right = count a'
balance :: Ord a => [a] -> Tree a
balance (a:[]) = Leaf a
balance ax = Node (balance left) (balance rigth)
    where 
      size = (length ax) `div` 2
      left = take size ax
      rigth = drop size ax
