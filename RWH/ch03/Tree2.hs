module Tree2 where
data Tree2 a = Maybe a (Maybe (Tree2 a)) (Maybe (Tree2 a))
             | Nobody
              deriving (Show,Eq,Ord)
-- t0 = Nobody
-- t1 = Just 1 t0 t0
-- t1 = Node 1 Empty Empty
-- t2 = Node 2 Empty Empty
-- t3 = Node 3 t1 t2
-- t4 = Node 4 t3 t1
-- left :: Tree2 a -> Tree2 a
-- left Empty = Empty
-- left (Node x l r) = l
-- right :: Tree2 a -> Tree2 a
-- right Empty = Empty
-- right (Node x l r) = r