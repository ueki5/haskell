module Pcc.Comb where

-- 順列を作る
perm :: [a] -> Int -> [[a]]
perm seed n = unfold [[]] seed n (splits div1)

-- 分割方法１　1要素、それ以外のすべての要素
div1 :: [a] -> Int -> (a, [a])
div1 ns pos = (head t,h ++ (tail t))
  where
    h = take (pos - 1) ns
    t = drop (pos - 1) ns

-- 組み合わせを作る
comb :: [a] -> Int -> [[a]]
comb seed n = unfold [[]] seed n (splits div2)

-- 分割方法２　1要素、それ以降にあらわれるすべての要素
div2 :: [a] -> Int -> (a, [a])
div2 ns pos = (head t,tail t)
  where
    h = take (pos - 1) ns
    t = drop (pos - 1) ns
-- 配列展開
unfold :: [[a]] -> [a] -> Int -> ([a] -> [(a, [a])]) -> [[a]]
unfold ass _  0 _ = ass
unfold ass [] n _ = fail "種のリストが足りません"
unfold ass seed n f = ass >>= \as' -> 
                   f seed >>= \(s',seed')  -> 
                   unfold [(as' ++ [s'])] seed' (n - 1) f
-- 入力配列→入力配列へ付加する要素,次処理の入力となる配列
splits :: ([a] -> Int -> (a, [a])) -> [a] -> [(a, [a])]
splits f ns = [1 .. length ns] >>= \pos ->
            return $ f ns pos
