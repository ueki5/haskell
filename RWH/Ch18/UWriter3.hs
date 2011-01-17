module Ch18.UWriter3 
       (
         Log,
         addn,
         execLog
       ) where
import Control.Monad

data Log a = Log {execLog::(a, String)} deriving (Show)
instance Monad Log where
  return a = Log (a, [])
  m >>= k = let (a, s) = execLog m
                n = k a
                (b, s') = execLog n
            in Log (b, s ++ "," ++ s')
calc :: Int -> Log Int
calc n = Log ( 2 * n, show n)
addn :: Int -> Int -> Log Int
addn n m = Log (n + m, show m )
go :: Int -> Log Int
go n = return n 
       >>= addn 1
       >>= addn 2
       >>= addn 3
