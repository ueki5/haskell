module Prettify where
import Numeric(showHex)
import Data.Bits(shiftR, (.&.))
import Data.Char(ord)
import SimpleJSON(JValue(..))
data Doc = Empty
         | Char Char
         | Text String
         | Line
         | Concat Doc Doc
         | Union Doc Doc
           deriving (Show, Eq)
empty :: Doc
empty = Empty
char :: Char -> Doc
char c = Char c
string :: String -> Doc
string = enclose '"' '"' . hcat . map oneChar
enclose :: Char -> Char -> Doc -> Doc
enclose left right x = char left <> x <> char right
(<>) :: Doc -> Doc -> Doc
Empty <> y = y
x <> Empty = x
a <> b = a `Concat` b
-- char :: Char -> Doc
-- char c = Char c
text :: String -> Doc
text "" = Empty
text s = Text s
double :: Double -> Doc
double d = Text (show d)
line :: Doc
line = Line

oneChar :: Char -> Doc
oneChar c = case lookup c simpleEscapes of
              Just r -> text r
              Nothing | mustEscape c -> hexEscape c
                      | otherwise -> char c
    where mustEscape c = c < ' ' || c == '\x7f' || c > '\xff'
simpleEscapes :: [(Char, String)]
simpleEscapes = zipWith ch "\b\n\f\r\t\\\"/" "bnfrt\\\"/"
    where ch a b = (a, ['\\',b])
hexEscape :: Char -> Doc
hexEscape c | d < 0x10000 = smallHex d
            | otherwise = astral (d - 0x10000)
    where d = ord c
smallHex :: Int -> Doc
smallHex x = text "\\u"
          <> text (replicate (4 - length h) '0')
          <> text h
             where h = showHex x ""
astral :: Int -> Doc
astral n = smallHex (a + 0xd800) <> smallHex (b + 0xdc00)
    where a = (n `shiftR` 10) .&. 0x3ff
          b = n .&. 0x3ff
series :: Char -> Char -> (a -> Doc) -> [a] -> Doc
series open close item = enclose open close
                       . fsep . punctuate (char ',') . map item
fsep :: [Doc] -> Doc
fsep [] = Empty
fsep [d] = d
fsep (d:ds) = d <> Line <> fsep ds
punctuate :: Doc -> [Doc] -> [Doc]
punctuate _ [] = []
punctuate p [d] = [d]
punctuate p (d:ds) = (d <> p) : punctuate p ds
------------------------------------------------------------
hcat :: [Doc] -> Doc
hcat [] = Empty
hcat (d:ds) = d <> hcat ds
