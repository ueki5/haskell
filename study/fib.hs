-- fib :: [Integer]
-- fib = fib' (0, 1) [0, 1]
-- fib' :: (Integer, Integer) -> [Integer] -> [Integer]
-- fib' (n, n') ns = fib' (n', n + n') (ns ++ [n + n'])
-- fib :: Integer -> Integer
-- fib 1 = 0
-- fib 2 = 1
-- fib (n + 2) = (fib n) + (fib (n + 1))
fib :: [Integer]
fib = [0 ,1] ++ [n1 + n2 | (n1,n2) <- zip fib (tail fib)]