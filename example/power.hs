plus n = (\x -> x + n)
plus1 = plus 1

-- ŠKæ‚P
power n = case n of
        0 -> 1
        otherwise -> n * (power (n - 1))

-- ŠKæ‚Q
power2 0 = 1
power2 n = n * (power (n - 1))

-- ŠKæ‚R
power3 n = if n == 0 
    then 1 
    else n * (power3 (n - 1))

-- ŠKæ‚S
power4 n 
    | n == 0 = 1 
    | otherwise = n * (power3 (n - 1))
