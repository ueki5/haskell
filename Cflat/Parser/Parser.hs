module Cflat.Parser.Parser where
import Control.Monad
import Data.Char
import Cflat.Type.Type

-- Parser
data Parser a = Parser {execParser::String -> Maybe (a, String)}
instance Monad Parser where
  return v = Parser $ \inp -> Just (v, inp)
  p >>= f = Parser $ \inp -> case parser p inp of
                                Nothing -> Nothing
                                Just (v, out) -> parser (f v) out
parser :: Parser a -> String -> Maybe (a, String)
parser = execParser
failure :: Parser a
failure = Parser $ \inp -> Nothing
item :: Parser Char
item = Parser $ \inp -> case inp of
  [] -> Nothing
  (c:cs) -> Just (c, cs)
-- ‚Ç‚¿‚ç‚©‚ª¬Œ÷‚µ‚½ê‡‚ÉÌ—p
(+++) :: Parser a -> Parser a -> Parser a
p +++ q = Parser $ \inp -> case parser p inp of
                      Nothing -> parser q inp
                      Just (v, out) -> Just (v, out)
-- ‘o•û‚ª¬Œ÷‚µ‚½ê‡‚ÉÌ—p
(&&&) :: Parser a -> Parser a -> Parser a
p &&& q = Parser $ \inp -> case parser p inp of
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
token :: Parser a -> Parser a
token p = do
  space
  cs <- p
  space
  return cs

int :: Parser Type
int = do
  cs <- token $ many1 digit
  return $ TpInt (read cs)
opr :: Parser Operator
opr = do
    operator <- token $ char '+' +++ char '-'
    case operator of
      '+' -> return Plus
      '-' -> return Minus
formula :: Parser Formula
-- formula = do
--   arg1 <- int
--   op   <- opr
--   arg2 <- int
--   return $ Op op (Tp arg1) (Tp arg2)
formula = do
            arg1 <- int
            op   <- opr
            frm  <- formula1
            return $ Op op (Tp arg1) frm
           +++ 
          do
            arg1 <- int
            return $ Tp arg1
formula1 :: Parser Formula
formula1 = formula
apply :: (a -> b) -> Maybe (a,String) -> Maybe b
apply _ Nothing = Nothing
apply f (Just (a,s)) = Just (f a)
calc :: Formula -> Int
calc (Tp (TpInt n)) = n
calc (Op Plus form1 form2) = (calc form1) + (calc form2)
calc (Op Minus form1 form2) = (calc form1) - (calc form2)
eval :: Parser Formula -> String -> Maybe Int
eval p s = apply calc (parser p s)