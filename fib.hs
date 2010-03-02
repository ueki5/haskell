-- あいうえおかきくけこ
fib = 0:1:(zipWith (+) fib (tail fib))
sq = [1..]
sq1 = iterate (1+) 1
sq2 = iterate (-1+) 0
fact n = product [1..n]