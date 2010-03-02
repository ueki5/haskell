import List
import Puzzle

data Kakro = K Int | W Int Int

instance Eq Kakro where
  (K x)   == (K y)   = x == y
  (W _ _) == (W _ _) = True
  _       == _       = False

instance Show Kakro where
  show (K x)   = show x
  show (W v h) = show v ++ "\\" ++ show h

instance Read Kakro where
  readsPrec _ s = case break ('\\'==) s of
	            (_,[])  -> [(K (read s), "")]
                    (v,_:h) -> [(W (read v) (read h),"")]

instance Puzzle Kakro where
  puzzleElm     = map K [1 .. 9]
  vacant = K 0
  candidate b (i,j) = intersect vcand hcand
    where vcand = nub (concat vcands) \\ vfixeds      -- ���������θ���
          hcand = nub (concat hcands) \\ hfixeds      -- �襳�����θ���
          vcands = filter (include vfixeds) $ kakroCalc vwa (length vgroup)
          hcands = filter (include hfixeds) $ kakroCalc hwa (length hgroup)
          vfixeds = filter (not . (vacant ==)) vgroup
          hfixeds = filter (not . (vacant ==)) hgroup
	  (W vwa _, vgroup) = getGroup j (transpose b !! i)
          (W _ hwa, hgroup) = getGroup i (b !! j)

getGroup :: Int -> [Kakro] -> (Kakro,[Kakro])
getGroup i ks = case splitAt i ks of
                  (ss,ts) -> case break (W 0 0 ==) ts of
                               (us,_) -> case break (W 0 0 ==) (reverse ss) of
                                           (rs,w:_) -> (w,reverse rs++us)

include :: Eq a => [a] -> [a] -> Bool        -- ��ʬ���礫��Ƚ�ꤹ��Ҹ�
include xs ys = null (xs \\ ys)

comb :: [a] -> Int -> [[a]]                  -- �ȹ礻������
comb _  0 = [[]]
comb [] _ = []
comb (x:xs) n = [ (x:xs') | xs' <- comb xs (n-1) ] ++ comb xs n

kakroCalc :: Int       -- ��
          -> Int       -- �ޥ��ܤο�
          -> [[Kakro]] -- ���ꤵ�줿�¤ˤʤ�Kakro����(1����9�ޤ�)���ȹ礻
kakroCalc s n = [ xs | xs <- comb puzzleElm n, s == sum (map (\ (K x) -> x) xs) ]

kakro :: Board Kakro -> [Board Kakro]
kakro = solve

instance Show (Board Kakro) where
  show = showBoard

instance Read (Board Kakro) where
  readsPrec _ s = [(readBoard s, "")]

sample :: Board Kakro
sample = read sampledata

sampledata :: String
sampledata 
 = unlines
   ["0\\0  17\\0  15\\0  23\\0  35\\0   0\\0   9\\0  15\\0  13\\0  11\\0"
   ,"0\\30  0      0      0      0      0\\22  0      0      0      0   "
   ,"0\\28  0      0      0      0     16\\10  0      0      0      0   "
   ,"0\\0  21\\0  23\\34  0      0      0      0      0      6\\0   7\\0"
   ,"0\\16  0      0     33\\17  0      0     16\\7   0      0      0   "
   ,"0\\29  0      0      0      0      7\\11  0      0      0      0   "
   ,"0\\22  0      0      0      8\\4   0      0      8\\4   0      0   "
   ,"0\\0  17\\0   8\\16  0      0      0      0      0      3\\0  10\\0"
   ,"0\\17  0      0      0      0      0\\14  0      0      0      0   "
   ,"0\\29  0      0      0      0      0\\11  0      0      0      0   "]
