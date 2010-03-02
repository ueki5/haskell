factorial         :: Integer -> Integer
    {-factorialの型はIntegerを貰いIntegerを返す-}
factorial n       = if n == 0 then 1
                              else n * factorial (n - 1)
