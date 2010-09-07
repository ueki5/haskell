module Ch14.Logger
    (
     Logger
     , Log
     , runLogger
     , record
    ) where
import Control.Monad
type Log = [String]
runLogger :: Logger a -> (a, Log)
runLogger = execLogger
record :: String -> Logger ()
record s = Logger ((), [s])
newtype Logger a = Logger { execLogger :: (a, Log)}
    deriving (Show)

instance Monad Logger where
    return a = Logger (a, [])
    m >>= k = let (a, w) = execLogger m
                  n = k a
                  (b, x) = execLogger n
              in Logger (b, w ++ x)

charClass :: String -> Logger String
charClass (']':cs) = (']':) `liftM` globToRegex' cs
charClass (c:cs) = (c:) `liftM` charClass cs
globToRegex' :: String -> Logger String
globToRegex' [] = return []
globToRegex' ('[':'!':c:cs) = do
    record "character class, negative!"
    ds <- charClass cs
    return ("[^" ++ c:ds)
globToRegex' ('[':c:cs) = do
    record "character class"
    ds <- charClass cs
    return ("[" ++ c:ds)
globToRegex' ('[':_) = do
    fail "unterminated character class"
globToRegex' ('*':cs) = do
    record "kleene star"
    ds <- globToRegex' cs
    return (".*" ++ ds)
globToRegex' ('?':cs) = record "any" >>
                        globToRegex' cs >>= \ds -> 
                        return ('.':ds)
globToRegex' (c:cs) = record "->" >>
                      globToRegex' cs >>= \ds -> 
                      return (c:ds)
globToRegex cs = globToRegex' cs >>= \ds ->
                 return ('^':ds)
-- list2list :: [a] -> [b] -> [(a, b)]
-- list2list xs ys = [(x, y) | x <- xs, y <- ys]
list2list xs ys = do 
  x <- xs
  y <- ys
  return (x ,y)
