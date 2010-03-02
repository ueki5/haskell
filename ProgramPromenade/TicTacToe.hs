module TicTacToe where

import Data.Char	( digitToInt )
import Data.Tree	( Tree(..) )

-- �����n�Ɋ܂܂�郉�C�u�����Ɠ��l�C���O�̃��W���[���� import �錾�Ŏ�荞��
import Mark		( Mark(..), isEmpty, next )
import Position

size :: Tree a -> Int				-- �߂̌������߂�
size (Node x ts) = 1 + sum (map size ts)

depth :: Tree a -> Int				-- �؂̐[�������߂�
depth (Node x []) = 0
depth (Node x ts) = 1 + maximum (map depth ts)

-- ���ׂĂ̕����؂� f ��K�p�������̂���\�������V���Ȗ؂����
mapSubTree :: (Tree a -> b) -> Tree a -> Tree b
mapSubTree f n@(Node x ts) = Node (f n) (map (mapSubTree f) ts)

-- x �����ɁC�J��Ԃ� f ��K�p���Ă��������̂��q�Ƃ���؂����
repTree :: (a -> [a]) -> a -> Tree a
repTree f x = Node x (map (repTree f) (f x))


type GTree = Tree Position

gameTree :: Position -> GTree
gameTree pos = repTree allMoves pos

prune :: Int -> Tree a -> Tree a
prune 0     (Node x  _) = Node x []
prune (m+1) (Node x ts) = Node x (map (prune m) ts)


static :: Position -> Int
static (Pos p pss) = sum [ eval p line | line <- allLines pss ]


eval :: Mark -> [Mark] -> Int
eval p qs = eval' (count p qs)
  where
    count :: Mark -> [Mark] -> (Int,Int)
    count p [] = (0,0)
    count p (q:qs) 
      | q == p    = (a+1,b)
      | q == p'   = (a,b+1)
      | otherwise = (a,b)
      where (a,b) = count p qs
	    p' = next p
    eval' :: (Int,Int) -> Int
    eval' (0,0) = 0
    eval' (a,0) = 10^a
    eval' (0,b) = -10^b
    eval' (_,_) = 0


dynamic :: Int -> Position -> Position
dynamic m pos = select_pos (-x2) ts2 ts1
  where n1@(Node x1 ts1) = (prune m . gameTree) pos
	Node x2 ts2 = (mapSubTree minimax  . fmap static) n1  -- minimax��
--	Node x2 ts2 = (mapSubTree minimax' . fmap static) n1  -- ��-����(��q)

type ITree = Tree Int

select_pos :: Int -> [ITree] -> [GTree] -> Position
select_pos u (Node v _ : its) (Node p _ : gts)
  = if u==v then p else select_pos u its gts

minimax :: ITree -> Int		-- ����̓��_���ŏ��ɂ���悤�Ȏ��I��
minimax (Node x []) = x
minimax (Node x ts) = -1 * minimum (map minimax ts)


minimax' :: ITree -> Int
minimax' = bmx (-max_sval) max_sval
  where
    max_sval = 2^31 - 1		-- as Infinity

bmx :: Int -> Int -> ITree -> Int
bmx a b (Node x []) = (a `max` x) `min` b
bmx a b (Node x ts) = cmx a b ts

cmx :: Int -> Int -> [ITree] -> Int
cmx a b []     = a
cmx a b (t:ts) = if a' == b then a' else cmx a' b ts
		   where a' = - bmx (-b) (-a) t


main, main' :: IO ()
main  = tictactoe (mkPos 3 3) O
main' = tictactoe (mkPos 3 3) X

tictactoe :: Position -> Mark -> IO ()
tictactoe pos p0
  = do putStr $ show pos
       (if p0 == O then loop0 else loop1) pos
  where
    loop0 pos			-- ���[�U�̎��
      = do putStr "XY : "
           (c1:c2:_) <- getLine
	   let [x,y] = map digitToInt [c1,c2]
	   let pos' = updatePosition (x-1,y-1) pos
	   putStr $ show pos'
	   finGame loop1 pos'
    loop1 pos			-- �v�Z�@�̎��
      = do putStr "\n"
           let pos' = dynamic prune_val pos
	   putStr $ show pos'
	   finGame loop0 pos'
	where prune_val = 5
    finGame next_loop pos
      | winlosegame pos = putStrLn $ "You " ++ if p==p0 then "lose." else "win!"
      | drawgame pos    = putStrLn "Game is draw."
      | otherwise       = next_loop pos
      where p = turn pos
