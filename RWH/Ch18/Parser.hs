module Ch18.Parser where
import Control.Monad

data Parser a = ParserD {execParser::String -> ([a], String)}
instance Monad Parser where
  return a = ParserD (\x-> ([a], x))
  m >>= k = k $ snd (execParser m)