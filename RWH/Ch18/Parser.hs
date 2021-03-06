module Ch18.Parser where
import Control.Monad
import Data.Char

-- Parser
data Element = UInt Int
             | Plus
             | Minus
             | Mult
             | Div
data Parser a = ParserD {execParser::String -> Maybe (a, String)}
instance Monad Parser where
  return v = ParserD $ \inp -> Just (v, inp)
  p >>= f = ParserD $ \inp -> case parser p inp of
                                Nothing -> Nothing
                                Just (v, out) -> parser (f v) out
parser :: Parser a -> String -> Maybe (a, String)
parser = execParser
failure :: Parser a
failure = ParserD $ \inp -> Nothing
item :: Parser Char
item = ParserD $ \inp -> case inp of
  [] -> Nothing
  (c:cs) -> Just (c, cs)
(+++) :: Parser a -> Parser a -> Parser a
p +++ q = ParserD $ \inp -> case parser p inp of
                      Nothing -> parser q inp
                      Just (v, out) -> Just (v, out)
(&&&) :: Parser a -> Parser a -> Parser a
p &&& q = ParserD $ \inp -> case parser p inp of
                      Nothing -> Nothing
                      Just (v, out) -> case parser q inp of
                        Nothing -> Nothing
                        Just (v', out') -> if out == out' then Just (v', out')
                                                      else Nothing
                                         
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
space :: Parser ()
space = do
  many (sat isSpace)
  return ()
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
nat :: Parser Int
nat = do
  cs <- many1 digit
  return $ read cs
ident :: Parser String
ident = do
    head <- letter
    tail <- many alphanum
    return (head:tail)
constant :: Parser String
constant = do
    head <- letter &&& upper
    tail <- many alphanum
    return (head:tail)

operator :: Parser String
operator = do
    op <- char '+' +++ char '-' +++ char '*' +++ char '/' +++ char '='
    return [op]
token :: Parser a -> Parser a
token p = do
  space
  cs <- p
  space
  return cs
op :: Parser String
op = token operator
int :: Parser Int
int = token nat
formula :: Parser (Int, String, Int)
formula = do
  arg1 <- int
  space
  opr <- op
  space
  arg2 <- int
  return (arg1, opr, arg2)