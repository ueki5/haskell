module Tree2 where
data Tree2 a = Node a (Maybe (Tree2 a)) (Maybe (Tree2 a))
              deriving (Show,Eq,Ord)
t0 = Node 0 Nothing Nothing
t1 = Node 1 (Just t0) (Just t0)
t2 = Node 2 (Just t0) (Just t1)
t3 = Node 3 (Just t1) (Just t2)
t4 = Node 4 (Just t2) (Just t3)
-- left :: Tree2 a -> Tree2 a
-- left Empty = Empty
-- left (Node x l r) = l
-- right :: Tree2 a -> Tree2 a
-- right Empty = Empty
-- right (Node x l r) = r