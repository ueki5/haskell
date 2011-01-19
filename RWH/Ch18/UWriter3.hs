module Ch18.UWriter3 (Log,execLog) where
import Control.Monad

data Log a = LogD {execLog::(a, String)} deriving (Show)
instance Monad Log where
  return a = LogD (a, [])
  m >>= k = let (a, s ) = execLog m
                n = k a
                (b, s') = execLog n
            in LogD (b, s ++ "," ++ s')
calc :: Int -> Log Int
calc n = LogD ( 2 * n, show n)
go :: Int -> Log Int
go n = return n 
       >>= calc
       >>= calc
       >>= calc

data Log1 a = LogD1 {execLog1::(a, [String])} deriving (Show)
instance Monad Log1 where
  return a = LogD1 (a, [])
  m >>= k = let (a, s ) = execLog1 m
                n = k a
                (b, s') = execLog1 n
            in LogD1 (b, s ++ s')
calc1 :: Int -> Log1 Int
calc1 n = LogD1 ( 2 * n, [show n])
go1 :: Int -> Log1 Int
go1 n = return n 
       >>= calc1
       >>= calc1
       >>= calc1
