module Ch14.Random where
import System.Random
import Control.Monad.State

rand :: IO Int
rand = getStdRandom (randomR (0, maxBound))

twoBadRandoms :: RandomGen g => g -> (Int, Int)
twoBadRandoms gen = (fst $ random gen, fst $ random gen)
go = twoBadRandoms `fmap` getStdGen
twoGoodRandoms :: RandomGen g => g -> ((Int, Int), g)
twoGoodRandoms g = let (r, g') = random g
                       (r', g'') = random g'
                   in ((r, r'), g'')
type RandomState a = State StdGen a
getRandom :: Random a => RandomState a
getRandom = get >>= \gen ->
            let (rnd, gen') = random gen
            in put gen' >> return rnd
getTwoRandoms :: Random a => RandomState (a, a)
getTwoRandoms = liftM2 (,) getRandom getRandom
-- get :: State s s
-- get = State $ \s -> (s, s)
-- put :: s -> State s ()
-- put s = State $ \_ -> ((), s)
runTwoRandoms :: IO (Int, Int)
runTwoRandoms = do
  oldState <- getStdGen
  let (result, newState) = runState getTwoRandoms oldState
  setStdGen newState
  return result