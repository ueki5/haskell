module Tree where
data Tree a = Node a (Tree a) (Tree a)
            | Empty
              deriving (Show,Eq,Ord)
t0 = Empty
t1 = Node 1 Empty Empty
t2 = Node 2 Empty Empty
t3 = Node 3 t1 t2
t4 = Node 4 t3 t1
left :: Tree a -> Tree a
left Empty = Empty
left (Node x l r) = l
right :: Tree a -> Tree a
right Empty = Empty
right (Node x l r) = r