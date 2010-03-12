module Parsing where
import Char
import Monad
-- infixr 5 +++
-- The monad of parsers
newtype Parser a =  P (String -> [(a,String)])
instance Monad Parser where
   return v = P (\inp -> [(v,inp)])
   p >>= f = P (\inp -> case parse p inp of
                          []        -> []
                          [(v,out)] -> parse (f v) out)
instance MonadPlus Parser where
   mzero =  P (\inp -> [])
   p `mplus` q =  P (\inp -> case parse p inp of
                               []        -> parse q inp
                               [(v,out)] -> [(v,out)])
--Basic parsers
failure:: Parser a
failure =  mzero
item:: Parser Char
item =  P (\inp -> case inp of
                     []     -> []
                     (x:xs) -> [(x,xs)])
parse:: Parser a -> String -> [(a,String)]
parse (P p) inp =  p inp
myp::Parser(Char, Char)
myp = do x <- item
         item
         y <- item
         return (x, y)