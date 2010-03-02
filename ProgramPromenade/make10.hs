{- きっぷ問題のプログラム (和田) -}

{- 計算結果は分数にして記憶する. このとき0で割ったときが問題になる.
1/0 は (1/1)/(0/1)=1/0 になり a をこれで割ると 0/1. これに10をたすとち
ょうど10になるが, すでに整数を3つ使っているので, 最後は9までしか足せず,
4つの整数の場合は10になることはない. -}

import List

data Tree = Tip Int | Fork Char Tree Tree       -- 先頭のCharは演算子 +,-,*,/

eval :: Tree -> (Int, Int)                      -- 2つのIntは分子と分母
eval (Tip x) = (x, 1)                           -- x を分数 x/1 とする
eval (Fork '+' x y) = (b * c + d * a, a * c)    -- b/a + d/c
                      where (b, a) = eval x
                            (d, c) = eval y

eval (Fork '-' x y) = (b * c - d * a, a * c)    -- b/a - d/c
                      where (b, a) = eval x
                            (d, c) = eval y

eval (Fork '*' x y) = (b * d, a * c)            -- b/a * d/c
                      where (b, a) = eval x
                            (d, c) = eval y

eval (Fork '/' x y) = (b * c, a * d)            -- b/a / d/c
                      where (b, a) = eval x
                            (d, c) = eval y

instance Show Tree where                        -- (x 演算子 y)と表示する
  show (Tip x) = show x
  show (Fork o x y) = "(" ++ show x ++ [' ',o,' '] ++ show y ++ ")"

evaltree :: Tree -> (Bool, Tree)                -- Boolは「10が作れる」
evaltree t = (d > 0 && n == d * 10, t) where (n,d) = eval t

trees :: (Char, Char, Char, [Int]) -> [(Bool, Tree)]    --Char,..は3つの演算子
trees x=                        -- 以下は3つの演算子を使う5つの式の形に対応
 evaltree(Fork o (Fork p (Fork q (Tip a) (Tip b)) (Tip c)) (Tip d)):
 evaltree(Fork o (Fork p (Tip a) (Fork q (Tip b) (Tip c))) (Tip d)):
 evaltree(Fork o (Fork p (Tip a) (Tip b)) (Fork q (Tip c) (Tip d))):
 evaltree(Fork o (Tip a) (Fork p (Fork q (Tip b) (Tip c)) (Tip d))):
 evaltree(Fork o (Tip a) (Fork p (Tip b) (Fork q (Tip c) (Tip d)))):[]
 where (o,p,q,[a,b,c,d])=x

perm    :: [Int]->[[Int]]               -- 順列を作る
perm [] = [[]]
perm xs = [x:ys | x<-xs, ys<- perm (delete x xs)]

good :: (Bool, Tree) -> Bool            -- filter用にBool部分を取り出す
good x = b where (b, t) = x

make10 :: [Int] -> [(Bool, Tree)]       -- 4整数をとり, (成否, 式)の列を返す
make10 x = filter good (concat (map trees y))   
   where y = [(o,p,q,as)|o<-ops, p<-ops, q<-ops, as<-perm x]
         ops = ['+','-','*','/']

-- 例
-- Main> make10 [3,4,7,8]     -- 10が作れる場合
-- [(True,((3 - (7 / 4)) * 8)),(True,(8 * (3 - (7 / 4))))]
-- Main> make10 [1,0,2,3]     -- 10が作れない場合
-- [] 
