data DumbTree = Empty | Fork DumbTree DumbTree

-- trees :: ����(>0)���顤DumbTree�Υꥹ�Ȥ�
trees :: Int -> [DumbTree]
trees 1 = [Empty]
trees n = concat [ joins ls rs | (ls,rs) <- [ lrs xs ys | (xs,ys) <- splits1 n ]]

-- splits1 :: ����(>0)���顤�¤����ο��ˤʤ�����(>0)������ȤΥꥹ�Ȥ�
splits1 :: Int -> [(Int,Int)] 
splits1 1 = []
splits1 n = (1,n-1) : [ (i+1,j) | (i,j) <- splits1 (n-1) ]

-- lrs :: 2�Ĥ��������顤���줾����礭����DumbTree�Υꥹ�Ȥ�����Ȥ�
lrs :: Int -> Int -> ([DumbTree],[DumbTree]) 
lrs xs ys = (trees xs, trees ys)

-- joins :: 2�Ĥ�DumbTree�Υꥹ�Ȥ��顤�����򤽤줾�캸���ˤ��DumbTree�Υꥹ�Ȥ�
joins :: [DumbTree] -> [DumbTree] -> [DumbTree]
joins ls rs = [ Fork l r | l <- ls, r <- rs ]

instance Show DumbTree where
  show Empty      = "0"
  show (Fork l r) = "(" ++ show l ++ "^" ++ show r ++ ")"

