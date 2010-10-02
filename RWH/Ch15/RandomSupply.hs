module Ch15.RandomSupply where

import Ch15.Supply
import System.Random hiding (next)
import Control.Arrow (first)

randomsIO :: Random a => IO [a]
randomsIO =
    getStdRandom $ \g ->
        let (a, b) = split g
        in (randoms a, b)
randomsIO' g = let (a, b) = split g
               in (randoms a, b)

randomsIO_golfed :: Random a => IO [a]
randomsIO_golfed = getStdRandom (first randoms . split) 
