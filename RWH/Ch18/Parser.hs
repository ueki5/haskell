module Ch18.Parser where
import Control.Monad

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
parser :: Parser a -> String -> [(a, String)]
parser = execParser

uekigo :: Parser (Char, Char, Char)
uekigo = item >>= \x -> 
         item >>= \y -> 
         item >>= \z ->
         return (x, y, z)

uekigo2 :: String -> [((Char, Char, Char), String)]
uekigo2 s = parser uekigo s

uekigo3 :: Parser Char
uekigo3 = item >>= \x ->
          return x

-- **************************************************************
-- data Parser a = ParserD {execParser :: String -> [(a, String)]}
-- instance Monad Parser where
--     return a = ParserD $ \s -> (a, [s])
--     m >>= k = let ns = execParser m ""
--                   case ns of
--                     [] -> ParserD 

-- **************************************************************
-- data Parser a = ParserD {execParser :: String -> a}
-- instance Monad Parser where
--     return a = ParserD $ \s -> a
--     -- m >>= k = f
--     --   where f s = k $ execParser m s
-- parser :: String -> [(Int, String)]
-- parser s = [(read s,s)]
-- int2par :: Int -> [(Int, String)]
-- int2par n = [(n, show n)]
-- compo :: String -> [(Int, String)]
-- compo s = let pairs = parser s
--           in  concat $ map (\x -> int2par $ fst x) pairs
