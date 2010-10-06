module Ch16.FormParse where

import Control.Monad (ap, liftM2)
import Text.ParserCombinators.Parsec
import Numeric (readHex)
p_query :: CharParser () [(String, Maybe String)]
p_query = p_pair `sepBy` char '&'
p_pair :: CharParser () (String, Maybe String)
-- p_pair = do
--     name <- many1 p_char
--     value <- optionMaybe (char '=' >> many p_char)
--     return (name, value)
p_pair = liftM2 (,) (many1 p_char) (optionMaybe (char '=' >> many p_char))

p_char :: CharParser () Char
p_char = oneOf urlBaseChars
         <|> (char '+' >> return ' ')
         <|> p_hex
p_hex :: CharParser () Char
p_hex = do
    char '%'
    a <- hexDigit
    b <- hexDigit
    let ((d, _):_) = readHex [a, b]
    return . toEnum $ d
urlBaseChars = ['a'..'z']++['A'..'Z']++['0'..'9']++"$-_.!*'(),"

parseQuery :: String -> Either ParseError [(String, Maybe String)]
parseQuery line = parse p_query "(unknown)" line
parsePair :: String -> Either ParseError (String, Maybe String)
parsePair line = parse p_pair "(unknown)" line
parseChar :: String -> Either ParseError Char
parseChar line = parse p_char "(unknown)" line
parseHex :: String -> Either ParseError Char
parseHex line = parse p_hex "(unknown)" line
