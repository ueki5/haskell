module Ch18.UWriter4 where
import Ch18.UWriter3
calc2 :: Int -> Log Int
calc2 n = return $ n * 2
go :: Int -> Log Int
go n = return n 
       >>= calc2
       >>= calc2
       >>= calc2
      