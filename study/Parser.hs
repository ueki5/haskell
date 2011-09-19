module Sutudy.Parser where
data Parser a = Parser {execParser::(String  -> Maybe (a, String))}
instance Monad Parser where
  return v = Parser (\inp -> Just (v,inp))
  ma >>= k = Parser $ \inp -> case execParser ma inp of 
    Nothing -> Nothing
    Just (a, out) -> execParser (k a) out 
parser = execParser
failure :: Parser a
failure = Parser (\inp -> Nothing)
item :: Parser Char
item = Parser $ \inp -> case inp of
  [] -> Nothing
  (c:cs) -> Just (c, cs)
sat :: (Char -> Bool) -> Parser Char
sat p = item >>= \c -> case p c of
  False -> failure
  True -> return c
  
