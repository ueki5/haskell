module Pcc.Ex2_1 where
fact :: Integer -> Integer
fact 0 = 1
fact n = n * fact (n - 1)
fib :: Integer -> Integer
fib 0 = 0
fib 1 = 1
fib n = fib (n - 1) + fib (n - 2)