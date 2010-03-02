import List (sort,group,nub)
import Ticket

-- 練習問題 3 の解答例

-- perm の代りに以下の gperm あるいは gperm' を使う

-- gperm: perm を使って作成したものから重複をとりのぞく
gperm :: Eq a => [a] -> [[a]]
gperm = nub . perm

-- gperm': 最初から重複したもの作成しない
gperm' :: Ord a => [a] -> [[a]]
gperm' xs = foldr (concatMap . merges) [[]] (group (sort xs))

merges :: [a] -> [a] -> [[a]]
merges [] ys = [ys]
merges xs [] = [xs]
merges xxs@(x:xs) yys@(y:ys)
 = map (x:) (merges xs yys) ++ map (y:) (merges xxs ys)

{- 
length (perm "122333") と length (gperm "122333") を評価した比べてみよ．

% ghci ex3.hs
   ___         ___ _
  / _ \ /\  /\/ __(_)
 / /_\// /_/ / /  | |      GHC Interactive, version 6.4, for Haskell 98.
/ /_\\/ __  / /___| |      http://www.haskell.org/ghc/
\____/\/ /_/\____/|_|      Type :? for help.

Loading package base-1.0 ... linking ... done.
Compiling Ticket           ( ./Ticket.hs, interpreted )
Compiling Main             ( ex3.hs, interpreted )
Ok, modules loaded: Main, Ticket.
*Main> length (perm "122333")
Loading package haskell98-1.0 ... linking ... done.
720
*Main> length (gperm "122333")
60 
-}