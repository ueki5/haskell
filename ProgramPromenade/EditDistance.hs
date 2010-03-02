import Control.Monad.State	( State, get, put, evalState )
import Data.FiniteMap		( FiniteMap, lookupFM, addToFM, emptyFM )
{- 最近の処理系なら以下も可．ただし lookup,insert は引数の順序が変わる
import Data.Map			( Map, lookup, insert, empty )
-}
-- transpose :: [[a]] -> [[a]]  リストのリストを行列とみなし転置
-- (例. transpose [[1,2,3],[4,5,6]] == [[1,4],[2,5],[3,6]])
import Data.List		( transpose )


type Table k v = FiniteMap k v
type Memo a b = State (Table a b) b

memoise :: Ord a => (a -> Memo a b) -> a -> Memo a b
memoise f x = do			    -- 1:
  table <- get				    -- 2: 表の取得
  case (lookupFM table x) of		    -- 3: 表の検索
    Just y  -> return y			    -- 4: 既に計算済みだった場合
    Nothing -> do fx <- f x		    -- 5: 未済のときは計算
		  table' <- get		    -- 6: 表の再取得
		  put (addToFM table' x fx) -- 7: 表に計算結果を格納
		  return fx		    -- 8: 結果を返す

runM :: (a -> Memo a b) -> a -> b
runM m v = evalState (m v) emptyFM


data OpC = Ins Char | Del Char | Subst Char Char | Keep Char
type IntOps = (Int, [OpC])

-- 各操作を，変換前の文字，操作を表わす文字，変換後の文字，と長さ 3 の文字列で表現
instance Show OpC where
  show (Ins c)     = ['-', 'v',  c ]
  show (Del c)     = [ c,  '^', '-']
  show (Subst c d) = [ c,  '!',  d ]
  show (Keep c)    = [ c,  ' ',  c ]


{-
-- 結果の表示なしバージョン
edM :: (String, String) -> Memo (String, String) Int
edM ([],       []) = return 0
edM (xs@(_:_), []) = return (length xs)
edM ([], ys@(_:_)) = return (length ys)
edM xys@(_, _) = memoise edM' xys	-- memoise の追加
  where
    edM' (xxs@(x:xs), yys@(y:ys)) = do
      a <- edM (xs,  ys)		-- 内部ではメモ化された edM を利用
      b <- edM (xxs, ys)
      c <- edM (xs, yys)
      return (minimum [ (if x==y then 0 else 1) + a, 1 + b, 1 + c ])

ed_memo :: (String, String) -> Int
ed_memo = runM edM
-}


-- 結果の表示ありバージョン
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

--  2 つ組の第一要素(fst)だけで順序を決定
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
