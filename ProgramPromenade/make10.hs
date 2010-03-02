{- ���ä�����Υץ���� (����) -}

{- �׻���̤�ʬ���ˤ��Ƶ�������. ���ΤȤ�0�ǳ�ä��Ȥ�������ˤʤ�.
1/0 �� (1/1)/(0/1)=1/0 �ˤʤ� a �򤳤�ǳ��� 0/1. �����10�򤿤��Ȥ�
�礦��10�ˤʤ뤬, ���Ǥ�������3�ĻȤäƤ���Τ�, �Ǹ��9�ޤǤ���­����,
4�Ĥ������ξ���10�ˤʤ뤳�ȤϤʤ�. -}

import List

data Tree = Tip Int | Fork Char Tree Tree       -- ��Ƭ��Char�ϱ黻�� +,-,*,/

eval :: Tree -> (Int, Int)                      -- 2�Ĥ�Int��ʬ�Ҥ�ʬ��
eval (Tip x) = (x, 1)                           -- x ��ʬ�� x/1 �Ȥ���
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

instance Show Tree where                        -- (x �黻�� y)��ɽ������
  show (Tip x) = show x
  show (Fork o x y) = "(" ++ show x ++ [' ',o,' '] ++ show y ++ ")"

evaltree :: Tree -> (Bool, Tree)                -- Bool�ϡ�10�������
evaltree t = (d > 0 && n == d * 10, t) where (n,d) = eval t

trees :: (Char, Char, Char, [Int]) -> [(Bool, Tree)]    --Char,..��3�Ĥα黻��
trees x=                        -- �ʲ���3�Ĥα黻�Ҥ�Ȥ�5�Ĥμ��η����б�
 evaltree(Fork o (Fork p (Fork q (Tip a) (Tip b)) (Tip c)) (Tip d)):
 evaltree(Fork o (Fork p (Tip a) (Fork q (Tip b) (Tip c))) (Tip d)):
 evaltree(Fork o (Fork p (Tip a) (Tip b)) (Fork q (Tip c) (Tip d))):
 evaltree(Fork o (Tip a) (Fork p (Fork q (Tip b) (Tip c)) (Tip d))):
 evaltree(Fork o (Tip a) (Fork p (Tip b) (Fork q (Tip c) (Tip d)))):[]
 where (o,p,q,[a,b,c,d])=x

perm    :: [Int]->[[Int]]               -- �������
perm [] = [[]]
perm xs = [x:ys | x<-xs, ys<- perm (delete x xs)]

good :: (Bool, Tree) -> Bool            -- filter�Ѥ�Bool��ʬ����Ф�
good x = b where (b, t) = x

make10 :: [Int] -> [(Bool, Tree)]       -- 4������Ȥ�, (����, ��)������֤�
make10 x = filter good (concat (map trees y))   
   where y = [(o,p,q,as)|o<-ops, p<-ops, q<-ops, as<-perm x]
         ops = ['+','-','*','/']

-- ��
-- Main> make10 [3,4,7,8]     -- 10��������
-- [(True,((3 - (7 / 4)) * 8)),(True,(8 * (3 - (7 / 4))))]
-- Main> make10 [1,0,2,3]     -- 10�����ʤ����
-- [] 
