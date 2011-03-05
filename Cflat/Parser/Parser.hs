module Cflat.Parser.Parser where
import Control.Monad
import Data.Char

-- Parser
data Parser a = Parser {execParser::String -> Maybe (a, String)}
instance Monad Parser where
  return v = Parser $ \inp -> Just (v, inp)
  p >>= f = Parser $ \inp -> case parser p inp of
                                Nothing -> Nothing
                                Just (v, out) -> parser (f v) out
parser :: Parser a -> String -> Maybe (a, String)
parser = execParser
failure :: Parser a
failure = Parser $ \inp -> Nothing
item :: Parser Char
item = Parser $ \inp -> case inp of
  [] -> Nothing
  (c:cs) -> Just (c, cs)
-- ‚Ç‚¿‚ç‚©‚ª¬Œ÷‚µ‚½ê‡‚ÉÌ—p
(+++) :: Parser a -> Parser a -> Parser a
p +++ q = Parser $ \inp -> case parser p inp of
                      Nothing -> parser q inp
                      Just (v, out) -> Just (v, out)
sat :: (Char -> Bool) -> Parser Char
sat p = do 
  x <- item
  if p x then return x
         else failure
digit :: Parser Char
digit = sat isDigit
lower :: Parser Char
lower = sat isLower
upper :: Parser Char
upper = sat isUpper
letter :: Parser Char
letter = sat isAlpha
alphanum :: Parser Char
alphanum = sat isAlphaNum
space :: Parser ()
space = do
  many (sat isSpace)
  return ()
char :: Char -> Parser Char
char c = sat (== c)
string :: String -> Parser String
string [] = return []
string (c:cs) = do
  x <- char c
  xs <- string cs
  return (x:xs)
many :: Parser a -> Parser [a]
many p = many1 p +++ return []
many1 :: Parser a -> Parser [a]
many1 p = do
  v <- p
  vs <- many p
  return (v:vs)
token :: Parser a -> Parser a
token p = do
  space
  cs <- p
  space
  return cs
data CompilationUnit = CompilationUnit ImportStmts TopDefs
                     deriving (Eq, Ord, Show)
type ImportStmts =  [ImportStmt]
data ImportStmt =  Import Names
                    deriving (Eq, Ord, Show)
type Names = String
type Name = String
type Dot = String
type TopDefs = [TopDef]
data TopDef = TopDef
             deriving (Eq, Ord, Show)
compilationUnit :: Parser CompilationUnit
compilationUnit = do
  imp_stmts <- importStmts
  top_defs <- topDefs
  return $ CompilationUnit imp_stmts top_defs
importStmts ::  Parser ImportStmts
importStmts =  many importStmt
importStmt :: Parser ImportStmt
importStmt = do
  imp <- token $ string "Import"
  nms <- names
  semc <- token $ string ";"
  return $ Import nms
names :: Parser Names
names = do  
  nm <- name
  do 
    dot <- token $ string "."
    nms <- names
    return $ nm ++ dot ++ nms
    +++ return nm
name :: Parser Name
name = token $ do
  alf <- letter
  alfnums <- many alphanum
  return $ alf:alfnums
topDefs :: Parser TopDefs
topDefs = do
  many1 topDef
topDef :: Parser TopDef
topDef = 
  defun
  +++ defvars
  +++ defconst
  +++ defstruct
  +++ defunion
  +++ typedef
defun = undefined
defvars = undefined
defconst = undefined
defstruct = undefined
defunion = undefined
typedef = undefined

