module Ch18.Parser where
import Control.Monad
import Data.Char

data Parser a = ParserD {execParser::String -> [(a, String)]}
instance Monad Parser where
  -- return :: a -> Parser a
  return v = ParserD $ \inp -> [(v, inp)]
  -- (>>=) :: Parser a -> (a -> Parser b) -> Parser b
  p >>= f = ParserD $ \inp -> case parser p inp of
                                [] -> []
                                ((v, out):ps) -> parser (f v) out
failure :: Parser a
failure = ParserD $ \inp -> []
item :: Parser Char
item = ParserD $ \inp -> case inp of
  [] -> []
  (c:cs) -> [(c, cs)]
check :: Parser Char
check = ParserD $ \inp -> case inp of
  [] -> []
  s@(c:cs) -> [(c, s)]
parser :: Parser a -> String -> [(a, String)]
parser = execParser
(+++) :: Parser a -> Parser a -> Parser a
p +++ q = ParserD $ \inp -> case parser p inp of
                      [] -> parser q inp
                      [(v, out)] -> [(v, out)]

sat :: (Char -> Bool) -> Parser Char
sat p = do 
  x <- item
  if p x then return x
         else failure
digit :: Parser Char
digit = sat isDigit
lower :: Parser Char
lower = sat isLower
upper :: Parser Char
upper = sat isUpper
letter :: Parser Char
letter = sat isAlpha
alphanum :: Parser Char
alphanum = sat isAlphaNum
char :: Char -> Parser Char
char c = sat (== c)
string :: String -> Parser String
string [] = return []
string (c:cs) = do
  x <- char c
  xs <- string cs
  return (x:xs)
many :: Parser a -> Parser [a]
many p = many1 p +++ return []
many1 :: Parser a -> Parser [a]
many1 p = do
  v <- p
  vs <- many p
  return (v:vs)
integer :: Parser String
integer = many1 digit
-- integer = do
--   x <- digit
--   xs <- integer
--   return (x:xs)

-- integer :: Parser String
-- integer = check >>= \x ->
--   if isDigit x 
--     then 
--       item >>= \x' ->
--       integer >>= \cs ->
--       return (x:cs)
--     else 
--       return []
-- integer = item >>= \x->
--             if isDigit x 
--             then 
--               item >>= \x'->
--                 if isDigit x' 
--                 then 
--                   item >>= \x''->
--                     if isDigit x'' 
--                     then 
--                       return (x:x':[x''])
--                     else 
--                       return (x:[x'])
--                 else 
--                   return [x]
--             else 
--               return []
-- integer = check >>= \x->
--             if isDigit x 
--             then 
--               item >>= \x' -> 
--               integer >>= \s ->
--               return (x:s)
--             else 
--               return []
