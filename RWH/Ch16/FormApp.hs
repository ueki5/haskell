module Ch16.FromApp where

import Control.Monad (ap, liftM2)
import Control.Applicative
import Text.ParserCombinators.Parsec hiding ((<|>))
import Numeric (readHex)

hexify :: (Enum a) => Char -> Char -> a
hexify a b = toEnum . fst . head . readHex $ [a,b]
urlBaseChars = ['a'..'z']++['A'..'Z']++['0'..'9']++"$-_.!*'(),"

p_hex :: CharParser () Char
p_hex = do
  char '%'
  a <- hexDigit
  b <- hexDigit
  let ((d, _):_) = readHex [a,b]
  return . toEnum $ d

a_hex :: CharParser () Char
a_hex = hexify <$> (char '%' *> hexDigit) <*> hexDigit
--     where hexify a b = toEnum . fst . head . readHex $ [a,b]
b_hex :: CharParser () Char
b_hex = do
  char '%'
  a <- hexDigit
  b <- hexDigit
  return $ hexify a b

p_char :: CharParser () Char
p_char = oneOf urlBaseChars
         <|> (char '+' >> return ' ')
         <|> p_hex
a_char :: CharParser () Char
a_char = oneOf urlBaseChars
         <|> (' ' <$ char '+')
         <|> p_hex
