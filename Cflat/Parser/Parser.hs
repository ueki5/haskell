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
-- どちらかが成功した場合に採用
(+++) :: Parser a -> Parser a -> Parser a
p +++ q = Parser $ \inp -> case parser p inp of
                      Nothing -> parser q inp
                      Just (v, out) -> Just (v, out)
-- 双方が成功した場合に採用
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
-- 数値
int :: Parser Type
int = do
  cs <- token $ many1 digit
  return $ TpInt (read cs)
fint :: Parser Formula
fint = do
  arg <- int
  return (Tp arg)
-- 演算子
plus :: Parser Operator
plus = token (char '+') >> return Plus
minus :: Parser Operator
minus = token (char '-') >> return Minus
mult :: Parser Operator
mult = token (char '*') >> return Mult
divide :: Parser Operator
divide = token (char '/') >> return Div
power :: Parser Operator
power = token (char '^') >> return Power

-- 足算、引算と掛算、除算をグループ化
opr1 :: Parser Operator
opr1 = plus +++ minus
opr2 :: Parser Operator
opr2 = mult +++ divide
opr3 :: Parser Operator
opr3 = power
opr :: Parser Operator
opr = opr1 +++ opr2 +++ opr3
-- レベル０（優先度最低：足算、引算　左結合）
form :: Parser Formula
form = do
  frm <- form1
  exprl frm opr1 form1
-- レベル１（優先度１：掛算、除算　右結合にしてみました）
form1 = do
  frm <- form2
  exprr frm opr2 form1
--レベル２（優先度２：べき乗　左結合）
form2 = do
  val <- fint
  exprl val opr3 fint
--左結合
exprl :: Formula -> Parser Operator -> Parser Formula -> Parser Formula
exprl frm o p = do
  op <- o
  frm' <- p
  exprl (Op op frm frm') o p
  +++ return frm
--右結合
exprr :: Formula -> Parser Operator -> Parser Formula -> Parser Formula
exprr frm o p = do
  op <- o
  frm' <- p
  return (Op op frm frm')
  +++ return frm