module Ch18.UWriter4 where
import Ch18.UWriter3
go :: Int -> Log Int
go n = return n 
       >>= addn 1
       >>= addn 2
       >>= addn 3
      