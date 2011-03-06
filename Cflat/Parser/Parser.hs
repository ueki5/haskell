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
compilationUnit :: Parser CompilationUnit
compilationUnit = do
  imp_stmts <- importStmts
  top_defs <- topDefs
  return $ CompilationUnit imp_stmts top_defs
type ImportStmts =  [ImportStmt]
data ImportStmt =  Import Names
                    deriving (Eq, Ord, Show)
importStmts ::  Parser ImportStmts
importStmts =  many importStmt
importStmt :: Parser ImportStmt
importStmt = do
  imp <- token $ string "Import"
  nms <- names
  semc <- token $ string ";"
  return $ Import nms
type Names = [Name] 
type Name = String
names :: Parser Names
names = do  
  nm <- name
  nms <- many $ separator "." name
  return (nm:nms)
name :: Parser Name
name = ident
type TopDefs = [TopDef]
data TopDef = TopDef
            | TopDefvars Defvars
            | TopDefun Defun
            | TopDefstruct Defstruct
            | TopDefunion Defunion
             deriving (Eq, Ord, Show)
topDefs :: Parser [TopDef]
topDefs = do
  many1 topDef
topDef :: Parser TopDef
topDef = 
  defun
  +++ defvars
  -- +++ defconst
  +++ defstruct
  +++ defunion
  +++ typedef
data Defun = Defun  Storage VarType Name Params Block
             deriving (Eq, Ord, Show)
defun :: Parser TopDef
defun = do
  strg <- storage
  tp <- vartyperef
  nm <- name
  prms <- parenthesis "(" params ")"
  blk <- block
  return (TopDefun  (Defun strg tp nm prms blk))
data Params = Void
            | FixedParam [Param]
            | UnfixedParam [Param]
              deriving (Eq, Ord, Show)
params = 
    do 
      prm <- param
      prms <- many (separator "," param)
      do
        unfixed <- separator "," (token $ string "...")
        return (UnfixedParam (prm:prms))
        +++ return (FixedParam (prm:prms))
    +++
    do
      v <- void
      return v
void = token $ do
  string "void"
  return Void
data Param = Param VarType Name
             deriving (Eq, Ord, Show)
param = do
  tp <- vartype
  nm <- name
  return $ Param tp nm
data Block = Block Defvarlist Stmts
               deriving (Eq, Ord, Show)
block = parenthesis 
  "{" 
  (do
     lst <- defvarlist
     ss <- stmts
     return $ Block lst ss)
  "}"
type Defvarlist = [Defvars]
defvarlist = many defvar
defvar = do
  tp <- vartype
  valnm <- defnamevalue
  valnms <- many (separator "," defnamevalue)
  semc <- token $ string ";"
  return (map (\(nm,  val) -> (Defvar NoStorage tp nm val)) (valnm:valnms))
type Stmts = [Stmt]
data Stmt = Stmt
               deriving (Eq, Ord, Show)
stmts = undefined
parenthesis :: String -> Parser a -> String -> Parser a
parenthesis l p r = do
  string l
  elm <- p
  string r
  return elm
type Defvars = [Defvar]
data Defvar = Defvar Storage VarType Name Value
             deriving (Eq, Ord, Show)
defvars :: Parser TopDef
defvars = do
  strg <- storage
  tp <- vartype
  valnm <- defnamevalue
  valnms <- many (separator "," defnamevalue)
  semc <- token $ string ";"
  return (TopDefvars  (map (\(nm,  val) -> (Defvar strg tp nm val)) (valnm:valnms)))
separator :: String -> Parser a -> Parser a
separator spr p = do
  s <- token $ string spr
  a <- p
  return a
data Storage = NoStorage
                    | Static
               deriving (Eq, Ord, Show)
storage = do
    strg <- token $ string "static"
    return Static
    +++ return NoStorage
data VarType = IntType
             | StrType
               deriving (Eq, Ord, Show)
vartype = token $ 
  do
    tp <- string "int"
    return IntType
  +++ 
  do 
    tp <-string "string"
    return StrType
vartyperef = vartype
defnamevalue :: Parser (Name,  Value)
defnamevalue = do
  nm <- name
  do
    eq <- token $ string "="
    val <- value
    return (nm, val)
    +++ return (nm, NoValue)
data Value = NoValue
           | Value String
               deriving (Eq, Ord, Show)
value = token $ do
   val <- many1 alphanum
   return (Value val)
defconst = undefined
data Defstruct = Defstruct Name MemberList
                 deriving (Eq, Ord, Show)
defstruct = do
  token $ string "struct"
  nm <- name
  memlst <- memberlist
  semc <- token $ string ";"
  return $ TopDefstruct (Defstruct nm memlst)
type MemberList = [Slot]
memberlist = many1 slot
data Slot = Slot VarType Name
                 deriving (Eq, Ord, Show)
slot = do
  tp <- vartype
  nm <- name
  semc <- token $ string ";"
  return $ Slot tp nm
data Defunion = Defunion Name MemberList
                 deriving (Eq, Ord, Show)
defunion = do
  token $ string "union"
  nm <- name
  memlst <- memberlist
  semc <- token $ string ";"
  return $ TopDefunion (Defunion nm memlst)
data Typedef = Typedef Typeref Ident
                 deriving (Eq, Ord, Show)
type Typeref = String
type Ident = String
ident :: Parser Ident
ident = token $ do
  alf <- letter
  alfnums <- many alphanum
  return $ alf:alfnums
typedef = undefined
