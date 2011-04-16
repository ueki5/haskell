module Test1 where
data Test1 = Test1 String String
             deriving (Eq, Ord, Show)
             -- deriving (Eq, Ord)
-- instance Show Test1 where
--          show (Test1 s1 s2)  = "Test1 " ++ s1 ++ " " ++ s2
instance Dump Test1
class Show a => Dump a where 
  dump :: a -> String
  dump = show
class MyMonad m where 
    return :: a -> m a
    (>>=) :: ( a -> m b) -> m a -> m b
