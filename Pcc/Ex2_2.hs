module Pcc.Ex2_2 where 
import Data.List
data Coin = Coin {unit::Int ,amount::Int}
            deriving (Eq,Ord)
input = [Coin 1 50,Coin 5 21,Coin 10 25,Coin 50 39,Coin 100 15 ,Coin 500 30]
solve :: Int -> [Coin] -> Int
solve n ns = snd $ foldl countCoin (n, 0) (reverse $ sort input)
countCoin :: (Int,Int) -> Coin -> (Int,Int)
countCoin (money, totalCount) coin = (remained, totalCount + count)
  where 
    remained = money - ((unit coin) * count)    
    count = min (money `div` (unit coin)) (amount coin)
