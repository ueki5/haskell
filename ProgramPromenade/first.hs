data DumbTree = Empty | Fork DumbTree DumbTree

-- trees :: 整数(>0)から，DumbTreeのリストへ
trees :: Int -> [DumbTree]
trees 1 = [Empty]
trees n = concat [ joins ls rs | (ls,rs) <- [ lrs xs ys | (xs,ys) <- splits1 n ]]

-- splits1 :: 整数(>0)から，和がその数になる整数(>0)の二つ組のリストへ
splits1 :: Int -> [(Int,Int)] 
splits1 1 = []
splits1 n = (1,n-1) : [ (i+1,j) | (i,j) <- splits1 (n-1) ]

-- lrs :: 2つの整数から，それぞれの大きさのDumbTreeのリストの二つ組へ
lrs :: Int -> Int -> ([DumbTree],[DumbTree]) 
lrs xs ys = (trees xs, trees ys)

-- joins :: 2つのDumbTreeのリストから，それらをそれぞれ左右にもつDumbTreeのリストへ
joins :: [DumbTree] -> [DumbTree] -> [DumbTree]
joins ls rs = [ Fork l r | l <- ls, r <- rs ]

instance Show DumbTree where
  show Empty      = "0"
  show (Fork l r) = "(" ++ show l ++ "^" ++ show r ++ ")"

