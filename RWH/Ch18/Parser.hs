module Ch18.Parser where
import Control.Monad

data Parser a = ParserD {execParser::String -> [(a, String)]}
instance Monad Parser where
  return v = ParserD $ \inp -> [(v, inp)]
  p >>= f = ParserD $ \inp -> case execParser p inp of
                                [] -> []
                                ((v, out):ps) -> execParser (f v) out
failure :: Parser a
failure = ParserD $ \inp -> []
item :: Parser Char
item = ParserD $ \inp -> case inp of
  [] -> []
  (c:cs) -> [(c, cs)]
parser :: Parser a -> String -> [(a, String)]
parser p inp = execParser p inp
-- uekigo :: a -> String -> [(a, String)]



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
