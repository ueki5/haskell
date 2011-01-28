module Ch18.Parser where
import Control.Monad

-- data Parser a = ParserD {execParser :: String -> [(a, String)]}
-- instance Monad Parser where
--     return a = ParserD $ \s -> (a, [s])
--     m >>= k = let ns = execParser m ""
--                   case ns of
--                     [] -> ParserD 

data Parser a = ParserD {execParser :: String -> a}
instance Monad Parser where
    return a = ParserD $ \s -> a
    m >>= k = f
      where f s = k $ execParser m s
