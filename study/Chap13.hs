module Chap13 where
import Prelude hiding (reverse)

data Nat =Zero |Succ Nat
add :: Nat -> Nat -> Nat
add Zero m = m
add (Succ n) m = Succ (add n m)

-- -- �ȉ��̐��������藧�����ؖ��������I
-- add n Zero = n
-- -- n = Zero�̎�
-- add Zero Zero = Zero
-- -- �ȉ������肵���ꍇ�A
-- add n Zero = n
-- -- �ȉ�����ɐ��藧���H�i���藧�j
-- add (Succ n) Zero = Succ (add n Zero)
--                     Succ n

-- -- ���R���̉��Z�Ɋւ��錋���@�������藧�����ؖ��������I
-- add x (add y z) = add (add x y) z
-- -- y = Zero�̎��A���藧��
-- add x (add Zero z) = add (add x Zero) z
-- add x z = add x z
-- -- y �̎��A���藧�Ɖ��肵���ꍇ�ASucc y �Ő��藧���H
-- add x (add y z) = add (add x y) z
-- add x (add (Succ y) z) = add (add x (Succ y)) z
-- -- �˂���

-- -- x = Zero�̎��A���藧��
-- add Zero (add y z) = add (add Zero y) z
-- add y z = add y z
-- -- x �̎��A���藧�Ɖ��肵���ꍇ�ASucc x �Ő��藧���H�ː��藧����
-- add x (add y z) = add (add x y) z
-- add (Succ x) (add y z) = add (add (Succ x) y) z
-- -- ��
-- Succ (add x (add y z))
-- Succ (add (add x y) z)
-- -- �E
-- add (add (Succ x) y) z
-- add (Succ (add x y)) z
-- Succ (add (add x y) z)

-- -- reverse
-- reverse :: [a] -> [a]
-- reverse [] = []
-- reverse (x:xs) = reverse xs ++ [x]
-- -- reverse �ɑ΂��A�ȉ��̐��������藧�����ؖ�����
-- reverse (reverse xs) = xs
-- -- xs = []�̎��A���藧��
-- reverse (reverse []) =reverse [] = []
-- -- xs �̂Ƃ����藧�Ɖ��肵�Ax:xs�̎����藧���H
-- reverse (reverse xs) = xs
-- reverse (reverse (x:xs)) = x:xs
-- -- ��
-- reverse (reverse xs ++ [x])
-- reverse [x] ++ reverse (reverse xs)
-- [x] ++ xs
