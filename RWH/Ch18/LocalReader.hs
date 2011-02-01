-- file: CH18/LocalReader.hs

-- class (Monad m) => MonadReader r m | m -> r where
--     ask   :: m r
--     local :: (r -> r) -> m a -> m a

module Ch18.LacalReader where
import Control.Monad.Reader

myName step = do
  name <- ask
  return (step ++ ", I am " ++ name)

localExample :: Reader String (String, String, String)
localExample = do
  a <- myName "First"
  b <- local (++"dy") (myName "Second")
  c <- myName "Third"
  return (a, b, c)
