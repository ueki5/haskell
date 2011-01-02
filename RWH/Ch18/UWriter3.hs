module Ch18.UWriter3 where
import Control.Monad

runLog :: Log a -> (a, String)
runLog = execLog
record :: String -> Log ()
record s = Log ((), s)
data Log a = Log {execLog::(a, String)} deriving (Show)
instance Monad Log where
  return a = Log (a, "")
  m >>= k = let (a, s) = execLog m
                n = k a
                (b, s') = execLog n
            in Log (b, s ++ "," ++ s')
calc :: Int -> Log Int
calc n = Log ( 2 * n, show n)
go :: Int -> Log Int
go n = return n >>= calc
     >>= calc
     >>= calc
go2 n = do
  a <- return n 
  b <- calc a
  c <- calc b
  d <- calc c
  return d
go3 :: String -> Log ()
go3 s = record s