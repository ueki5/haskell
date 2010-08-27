module TreeMap where
data Tree a = Leaf a
            | Node (Tree a) (Tree a)
              deriving (Show)
-- treeLengths (Leaf s) = Leaf (length s)
-- treeLengths (Node l r) = Node (treeLengths l)  (treeLengths r)
treeMap :: (a -> b) -> Tree a -> Tree b
treeMap f (Leaf s) = Leaf (f s)
treeMap f (Node l r) = Node (treeMap f l) (treeMap f r)
treeLengths = treeMap length
instance Functor Tree where
    fmap = treeMap
