module Ch16.Csv2 where
import Text.ParserCombinators.Parsec

csvFile = endBy line eol
line = sepBy cell (char ',')
cell = many (noneOf ",\n")
-- eol = string "\n\r" <|> string "\n"
-- eol = do
--   char '\n'
--   char '\r' <|> return '\n'
eol =     try (string "\n\r")
      <|> try (string "\r\n")
      <|> string "\n"
      <|> string "\r"
      <?> "end of line"
--       <|> fail "Couldn't find EOL"
parseCSV :: String -> Either ParseError [[String]]
parseCSV input = parse csvFile "(unknown)" input
