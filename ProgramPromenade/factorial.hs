factorial         :: Integer -> Integer
    {-factorial�η���Integer���㤤Integer���֤�-}
factorial n       = if n == 0 then 1
                              else n * factorial (n - 1)
