import Control.Monad.State	( State, get, put, evalState )
import Data.FiniteMap		( FiniteMap, lookupFM, addToFM, emptyFM )
{- �ŋ߂̏����n�Ȃ�ȉ����D������ lookup,insert �͈����̏������ς��
import Data.Map			( Map, lookup, insert, empty )
-}
-- transpose :: [[a]] -> [[a]]  ���X�g�̃��X�g���s��Ƃ݂Ȃ��]�u
-- (��. transpose [[1,2,3],[4,5,6]] == [[1,4],[2,5],[3,6]])
import Data.List		( transpose )


type Table k v = FiniteMap k v
type Memo a b = State (Table a b) b

memoise :: Ord a => (a -> Memo a b) -> a -> Memo a b
memoise f x = do			    -- 1:
  table <- get				    -- 2: �\�̎擾
  case (lookupFM table x) of		    -- 3: �\�̌���
    Just y  -> return y			    -- 4: ���Ɍv�Z�ς݂������ꍇ
    Nothing -> do fx <- f x		    -- 5: ���ς̂Ƃ��͌v�Z
		  table' <- get		    -- 6: �\�̍Ď擾
		  put (addToFM table' x fx) -- 7: �\�Ɍv�Z���ʂ��i�[
		  return fx		    -- 8: ���ʂ�Ԃ�

runM :: (a -> Memo a b) -> a -> b
runM m v = evalState (m v) emptyFM


data OpC = Ins Char | Del Char | Subst Char Char | Keep Char
type IntOps = (Int, [OpC])

-- �e������C�ϊ��O�̕����C�����\�킷�����C�ϊ���̕����C�ƒ��� 3 �̕�����ŕ\��
instance Show OpC where
  show (Ins c)     = ['-', 'v',  c ]
  show (Del c)     = [ c,  '^', '-']
  show (Subst c d) = [ c,  '!',  d ]
  show (Keep c)    = [ c,  ' ',  c ]


{-
-- ���ʂ̕\���Ȃ��o�[�W����
edM :: (String, String) -> Memo (String, String) Int
edM ([],       []) = return 0
edM (xs@(_:_), []) = return (length xs)
edM ([], ys@(_:_)) = return (length ys)
edM xys@(_, _) = memoise edM' xys	-- memoise �̒ǉ�
  where
    edM' (xxs@(x:xs), yys@(y:ys)) = do
      a <- edM (xs,  ys)		-- �����ł̓��������ꂽ edM �𗘗p
      b <- edM (xxs, ys)
      c <- edM (xs, yys)
      return (minimum [ (if x==y then 0 else 1) + a, 1 + b, 1 + c ])

ed_memo :: (String, String) -> Int
ed_memo = runM edM
-}


-- ���ʂ̕\������o�[�W����
edM :: (String, String) -> Memo (String, String) IntOps
edM ([],       []) = return (0, [])
edM (xs@(_:_), []) = return (length xs, map Del xs)
edM ([], ys@(_:_)) = return (length ys, map Ins ys)
edM xys@(_, _) = memoise edM' xys
  where
    edM' (xxs@(x:xs), yys@(y:ys)) = do
      (a, ops_a) <- edM (xs,  ys)
      (b, ops_b) <- edM (xxs, ys)
      (c, ops_c) <- edM (xs, yys)
      return (min3 (if x==y then (a, Keep x:ops_a)
			    else (1+a, Subst x y:ops_a))
		   (1+b, Ins y:ops_b)
		   (1+c, Del x:ops_c))

--  2 �g�̑��v�f(fst)�����ŏ���������
min3 :: Ord a => (a,b) -> (a,b) -> (a,b) -> (a,b)
min3 a b c = if a `le` b then a `min2` c else b `min2` c
  where
    (x, _) `le` (y, _) = x <= y
    min2 x y | x `le` y  = x
             | otherwise = y

ed_memo :: (String, String) -> IntOps
ed_memo = runM edM

ed_all :: (String, String) -> IO ()
ed_all xys = do
  putStrLn $ show d
  mapM_ putStrLn $ transpose $ map show ops
  where
    (d, ops) = ed_memo xys


main = ed_all ("mathematical games", "metamagical themas")
