module Ch16.FromApp where

import Control.Applicative
import Control.Monad (MonadPlus(..), ap, liftM2)
import Text.ParserCombinators.Parsec hiding (many, optional, (<|>))
import Numeric (readHex)

instance Applicative (GenParser s a) where
  pure = return
  (<*>) = ap
instance Alternative (GenParser s a) where
  empty = mzero
  (<|>) = mplus

hexify :: (Enum a) => Char -> Char -> a
hexify a b = toEnum . fst . head . readHex $ [a,b]
urlBaseChars = ['a'..'z']++['A'..'Z']++['0'..'9']++"$-_.!*'(),"

-- ----------------------------
a_query :: CharParser () [(String, Maybe String)]
a_query = a_pair `sepBy` char '&'
-- ----------------------------
a_pair :: CharParser () (String, Maybe String)
a_pair = liftA2 (,) (many1 a_char) (optionMaybe (char '=' *> many a_char))
-- ----------------------------
a_char :: CharParser () Char
a_char = oneOf urlBaseChars
         <|> (' ' <$ char '+')
         <|> a_hex
-- ----------------------------
a_hex :: CharParser () Char
a_hex = hexify <$> (char '%' *> hexDigit) <*> hexDigit

-- -- ----------------------------
-- p_query :: CharParser () [(String, Maybe String)]
-- p_query = p_pair `sepBy` char '&'
-- -- ----------------------------
-- p_pair :: CharParser () (String, Maybe String)
-- p_pair = liftM2 (,) (many1 p_char) (optionMaybe (char '=' >> many p_char))
-- -- ----------------------------
-- p_char :: CharParser () Char
-- p_char = oneOf urlBaseChars
--          <|> (char '+' >> return ' ')
--          <|> p_hex
-- -- ----------------------------
-- p_hex :: CharParser () Char
-- p_hex = do
--   char '%'
--   a <- hexDigit
--   b <- hexDigit
--   let ((d, _):_) = readHex [a,b]
--   return . toEnum $ d
