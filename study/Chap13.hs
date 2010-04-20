module Chap13 where
import Prelude hiding (reverse)

data Nat =Zero |Succ Nat
add :: Nat -> Nat -> Nat
add Zero m = m
add (Succ n) m = Succ (add n m)

-- -- 以下の性質が成り立つ事を証明したい！
-- add n Zero = n
-- -- n = Zeroの時
-- add Zero Zero = Zero
-- -- 以下を仮定した場合、
-- add n Zero = n
-- -- 以下が常に成り立つか？（成り立つ）
-- add (Succ n) Zero = Succ (add n Zero)
--                     Succ n

-- -- 自然数の加算に関する結合法則が成り立つ事を証明したい！
-- add x (add y z) = add (add x y) z
-- -- y = Zeroの時、成り立つ
-- add x (add Zero z) = add (add x Zero) z
-- add x z = add x z
-- -- y の時、成り立つと仮定した場合、Succ y で成り立つか？
-- add x (add y z) = add (add x y) z
-- add x (add (Succ y) z) = add (add x (Succ y)) z
-- -- ⇒だめ

-- -- x = Zeroの時、成り立つ
-- add Zero (add y z) = add (add Zero y) z
-- add y z = add y z
-- -- x の時、成り立つと仮定した場合、Succ x で成り立つか？⇒成り立った
-- add x (add y z) = add (add x y) z
-- add (Succ x) (add y z) = add (add (Succ x) y) z
-- -- 左
-- Succ (add x (add y z))
-- Succ (add (add x y) z)
-- -- 右
-- add (add (Succ x) y) z
-- add (Succ (add x y)) z
-- Succ (add (add x y) z)

-- reverse
reverse :: [a] -> [a]
reverse [] = []
reverse (x:xs) = reverse xs ++ [x]
-- -- reverse に対し、以下の性質が成り立つ事を証明せよ
-- reverse (reverse xs) = xs
-- -- xs = []の時、成り立つ
-- reverse (reverse []) =reverse [] = []
-- -- xs のとき成り立つと仮定し、x:xsの時成り立つか？
-- reverse (reverse xs) = xs
-- reverse (reverse (x:xs)) = x:xs
-- -- 左
-- reverse (reverse xs ++ [x])
-- reverse [x] ++ reverse (reverse xs)
-- [x] ++ xs
