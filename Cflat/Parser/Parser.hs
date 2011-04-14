module Cflat.Parser.Parser where
import System.IO
import Control.Monad
import Control.Exception (bracket)
import Data.Char

--parseFile
parseFile :: FilePath -> IO (Maybe AST)
parseFile = parseFile' compilationUnit
parseFile' :: Parser a -> FilePath -> IO (Maybe a)
parseFile' p path = applyFile func path
    where func s = case (parser p s) of
                     Just (ans, []) -> Just ans
                     _              -> Nothing
applyFile :: (String -> Maybe a) -> FilePath -> IO (Maybe a)
applyFile f path = bracket 
                 (openFile path ReadMode) 
                 hClose 
                 $ \inh -> do
                   s <-mainLoop inh
                   case commentoff CommentOff s of
                     Just s' -> return (f s')
                     _       -> return Nothing
applyFile :: (String -> a) -> FilePath -> IO a
applyFile f path = bracket 
                 (openFile path ReadMode) 
                 hClose 
                 $ \inh -> do
                   s <-mainLoop inh
                   return (f s)
mainLoop :: Handle -> IO String
mainLoop inh = do
  ineof <- hIsEOF inh
  if ineof
     then return []
     else do     
       c <- hGetChar inh 
       cs <- mainLoop inh
       return  (c:cs)
-- Parser
data Parser a = Parser {execParser::String -> Maybe (a,  String)}
instance Monad Parser where
  return v = Parser $ \inp -> Just (v, inp)
  p >>= f = Parser $ \inp -> case parser p inp of
                                Nothing -> Nothing
                                Just (v, out) -> parser (f v) out
parser :: Parser a -> String -> Maybe (a, String)
parser = execParser
data CommentStatus = CommentOff
                     | LineOn
                     | RegionOn
commentoff :: CommentStatus -> String -> Maybe String
commentoff CommentOff ('/':('/':cs)) = commentoff LineOn cs
commentoff CommentOff ('/':('*':cs)) = commentoff RegionOn cs
commentoff LineOn ('\n':cs) = commentoff CommentOff cs
commentoff LineOn ('\r':('\n':cs)) = commentoff CommentOff cs
commentoff RegionOn ('*':('/':cs)) = commentoff CommentOff cs
commentoff LineOn (c:cs) = commentoff LineOn cs
commentoff RegionOn (c:cs) = commentoff RegionOn cs
commentoff CommentOff (c:cs) = do
                               cs' <- commentoff CommentOff cs
                               Just (c:cs')
commentoff RegionOn []  = Nothing
commentoff _ [] = Just []

failure :: Parser a
failure = Parser $ \inp -> Nothing
item :: Parser Char
item = Parser $ \inp -> case inp of
  [] -> Nothing
  (c:cs) -> Just (c, cs)
-- ‚Ç‚¿‚ç‚©‚ª¬Œ÷‚µ‚½ê‡‚ÉÌ—p
(+++) :: Parser a -> Parser a -> Parser a
(+++) p q = Parser $ \inp -> case parser p inp of
                      Nothing -> parser q inp
                      Just (v, out) -> Just (v, out)
-- ‘o•û‚ª¬Œ÷‚µA‚©‚ÂÁ”ï‚µ‚½•¶Žš”‚ª“¯‚¶‚¾‚Á‚½ê‡‚ÉÌ—p
(&&&) :: Parser a -> Parser a -> Parser a
p &&& q = Parser $ \inp -> case parser p inp of
                      Nothing -> Nothing
                      Just (v, out) -> case parser q inp of
                        Nothing -> Nothing
                        Just (v', out') -> if length out == length out' then Just (v', out')
                                                      else Nothing
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
letter = sat (\x -> (isAlpha x) || (x == '_'))
ascii :: Parser Char
ascii = sat isAscii
str :: Parser Char
str = sat (/= '\"')
alphanum :: Parser Char
alphanum = sat (\x -> (isAlphaNum x) || (x == '_'))
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
data AST = AST ImportStmts TopDefs
                     deriving (Eq, Ord, Show)
compilationUnit :: Parser AST
compilationUnit = do
  imp_stmts <- importStmts
  top_defs <- topDefs
  return $ AST imp_stmts top_defs
type ImportStmts =  [ImportStmt]
data ImportStmt =  Import Names
                    deriving (Eq, Ord, Show)
importStmts ::  Parser ImportStmts
importStmts =  many importStmt
importStmt :: Parser ImportStmt
importStmt = do
  imp <- token $ string "import"
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
data TopDef = TopDefvars Defvars
            | TopDefun Defun
            | TopDefconst Defconst
            | TopDefstruct Defstruct
            | TopDefunion Defunion
            | TopDeftype Typedef
             deriving (Eq, Ord, Show)
topDefs :: Parser [TopDef]
topDefs = many topDef
topDef :: Parser TopDef
topDef = defun
         +++ topdefvar
         +++ defconst
         +++ defstruct
         +++ defunion
         +++ typedef
data Defun = Defun  Storage Typeref Name Params Block
             deriving (Eq, Ord, Show)
defun :: Parser TopDef
defun = do
  strg <- storage
  tp <- typeref
  nm <- name
  prms <- parentheses "(" params ")"
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
data Param = Param Typeref Name
             deriving (Eq, Ord, Show)
param = do
  tp <- typeref
  nm <- name
  return $ Param tp nm
data Block = Block Defvarlist Stmts
               deriving (Eq, Ord, Show)
block = parentheses 
        "{" (do
              lst <- defvarlist
              ss <- stmts
              return $ Block lst ss) "}"
type Defvarlist = [Defvars]
defvarlist = many defvar
defvar = do
  strg <- storage
  tp <- typeref
  valnm <- defnamevalue
  valnms <- many (separator "," defnamevalue)
  token $ string ";"
  return (map (\(nm,  val) -> (Defvar strg tp nm val)) (valnm:valnms))
type Stmts = [Stmt]
data Stmt = BlankLine
          | LabeledStmt Name Stmts
          | StmtExpr Expr
          | StmtBlock Block
          | IfStmt Expr Stmt Stmt
          | WhileStmt Expr Stmt
          | DoWhileStmt Expr Stmt
          | ForStmt Expr Expr Expr Stmt
          | SwitchStmt Expr CaseClauses
          | BreakStmt
          | ContinueStmt 
          | GotoStmt Name
          | ReturnStmt Expr
          | ReturnVoid
               deriving (Eq, Ord, Show)
data Expr = ExprAssign Assign
               | ExprOpAssign OpAssign
               | Expr1 Term
               | Expr2 Operator Expr Expr
               | Expr3 Operator Expr Expr Expr
               deriving (Eq, Ord, Show)
data ExprPair = ExprPair Operator Expr
             deriving (Eq, Ord, Show)
stmtbases :: Parser Stmts
stmtbases = do
  ss <- many stmtbase
  return ss
stmts :: Parser Stmts
stmts = do
  ss <- many stmt
  return ss
stmtbase :: Parser Stmt
stmtbase = blankline
  +++ exprstmt
  +++ stmtblock
  +++ ifstmt
  +++ whilestmt
  +++ dowhilestmt
  +++ forstmt
  +++ switchstmt
  +++ breakstmt
  +++ continuestmt
  +++ gotostmt
  +++ returnstmt
stmt :: Parser Stmt
stmt = labeledstmt
  +++ stmtbase
blankline :: Parser Stmt
blankline = do
  semi <- token $ string ";"
  return BlankLine
labeledstmt :: Parser Stmt
labeledstmt = do
  l <- name
  colon <- token $ string ":"
  s <- stmtbases
  return $ LabeledStmt l s
exprstmt :: Parser Stmt
exprstmt = do
  e <- expr
  token $ string ";"
  return $ StmtExpr e
expr :: Parser Expr
expr = assign
       +++ opassign
       +++ do
         e10 <- expr10
         return e10
data Assign = AssignExpr Term Expr
              deriving (Eq, Ord, Show)
assign :: Parser Expr
assign = do
  t <- term
  token $ string "="
  e <- expr
  return (ExprAssign (AssignExpr t e))
data Operator = TernaryOp
               | Pararell
               | DoubleAmpasand
               | GreaterThan
               | LessThan
               | GreaterOrEqual
               | LessOrEqual
               | Equal
               | NotEqual
               | Virticalbar
               | Circumflex
               | Ampersand
               | ShiftLeft
               | ShiftRight
               | Plus
               | Minus
               | Mult
               | Div
               | Mod
               deriving (Eq, Ord, Show)
operator9 :: Parser Operator
operator9 = do
  token $ string "||"
  return Pararell
operator8 :: Parser Operator
operator8 = do
  token $ string "&&"
  return DoubleAmpasand
operator7 :: Parser Operator
operator7 = do
  token $ string "=="
  return Equal
  +++ do
  token $ string "!="
  return NotEqual
  +++ do
  token $ string ">="
  return GreaterOrEqual
  +++ do
  token $ string ">"
  return GreaterThan
  +++ do
  token $ string "<="
  return LessOrEqual
  +++ do
  token $ string "<"
  return LessThan
operator6 :: Parser Operator
operator6 = do
  token $ string "|"
  return Virticalbar
operator5 :: Parser Operator
operator5 = do
  token $ string "^"
  return Circumflex
operator4 :: Parser Operator
operator4 = do
  token $ string "&"
  return Ampersand
operator3 :: Parser Operator
operator3 = do
  token $ string "<<"
  return ShiftLeft
  +++ do
  token $ string ">>"
  return ShiftRight
operator2 :: Parser Operator
operator2 = do
  token $ string "+"
  return Plus
  +++ do
  token $ string "-"
  return Minus
operator1 :: Parser Operator
operator1 = do
  token $ string "*"
  return Mult
  +++ do
  token $ string "/"
  return Div
  +++ do
  token $ string "%"
  return Mod
operator :: Parser Operator
operator = operator9
           +++ operator8
           +++ operator7
           +++ operator6
           +++ operator5
           +++ operator4
           +++ operator3
           +++ operator2
           +++ operator1
data OpAssignOp = PlusEqual
                | MinusEqual
                | MultEqual
                | DivEqual
                | ModEqual
                | AmpersandEqual
                | VirticalbarEqual
                | CircumflexEqual
                | ShiftLeftEqual
                | ShiftRightEqual
              deriving (Eq, Ord, Show)
opassignop :: Parser OpAssignOp
opassignop = do
  token $ string "+="
  return PlusEqual
  +++ do
  token $ string "-="
  return MinusEqual
  +++ do
  token $ string "*="
  return MultEqual
  +++ do
  token $ string "/="
  return DivEqual
  +++ do
  token $ string "%="
  return ModEqual
  +++ do
  token $ string "&="
  return AmpersandEqual
  +++ do
  token $ string "|="
  return VirticalbarEqual
  +++ do
  token $ string "^="
  return CircumflexEqual
  +++ do
  token $ string "<<="
  return ShiftLeftEqual
  +++ do
  token $ string ">>="
  return ShiftRightEqual
data OpAssign = OpAssign Term OpAssignOp Expr
              deriving (Eq, Ord, Show)
opassign :: Parser Expr
opassign = do
  t <- term
  o <- opassignop
  e <- expr
  return (ExprOpAssign (OpAssign t o e))
data Term = TermCast Typeref Term
          | TermUnary Unary
              deriving (Eq, Ord, Show)
term :: Parser Term
term = do
  tp <- parentheses "(" typeref ")"
  tm <- term
  return $ TermCast tp tm
  +++ do
    u <- unary
    return $ TermUnary u
data Unary = PrefixPlus Unary
           | PrefixMinus Unary
           | UnaryPlus Term
           | UnaryMinus Term
           | LogicalNegation Term
           | BitInversion Term
           | Dereference Term
           | AddressOperator Term
           | Sizeoftype Typeref
           | Sizeofunary Unary
           | UnaryPostfix Postfix
              deriving (Eq, Ord, Show)
unary :: Parser Unary
unary = 
   do
     token $ string "++"
     u <- unary
     return $ PrefixPlus u
   +++ do
     token $ string "--"
     u <- unary
     return $ PrefixMinus u
   +++ do
     token $ string "+"
     t <- term
     return $ UnaryPlus t
   +++ do
     token $ string "-"
     t <- term
     return $ UnaryMinus t
   +++ do
     token $ string "!"
     t <- term
     return $ LogicalNegation t
   +++ do
     token $ string "~"
     t <- term
     return $ BitInversion t
   +++ do
     token $ string "*"
     t <- term
     return $ Dereference t
   +++ do
     token $ string "&"
     t <- term
     return $ AddressOperator t
   +++ do
     token $ string "sizeof"
     tp <- parentheses "(" typeref ")"
     return $ Sizeoftype tp
   +++ do
     token $ string "sizeof"
     u <- unary
     return $ Sizeofunary u
   +++ do
     p <- postfix
     return $ UnaryPostfix  p
data Postfix = PostfixPrimary Primary
             | PostfixComb Primary [Postfix']
             deriving (Eq, Ord, Show)
data Postfix' = PostfixPlus
              | PostfixMinus
              | RefArray Expr
              | RefMember Name
              | RefByPointer Name
              | FuncCall Args
             deriving (Eq, Ord, Show)
postfix :: Parser Postfix
postfix = do
  p <- primary
  do
     ps <- many postfix'
     return (PostfixComb p ps)
     +++ return (PostfixPrimary p)
postfix' = 
    do
      token $ string "++"
      return PostfixPlus
    +++ do
      token $ string "--"
      return PostfixMinus
    +++ do
      e <- parentheses "[" expr "]"
      return (RefArray e)
    +++ do
      token $ string "."
      nm <- name
      return (RefMember nm)
    +++ do
      token $ string "->"
      nm <- name
      return (RefByPointer nm)
    +++ do
      a <- parentheses "(" args ")"
      return (FuncCall a)
data Args = ArgsExpr [Expr]
            deriving (Eq, Ord, Show)
args :: Parser Args
args = do
  e <- expr
  es <- many (separator "," expr)
  return $ ArgsExpr (e:es)
  +++ return (ArgsExpr [])
data Primary = DECIMAL String
             | UNSIGNEDDECIMAL String
             | LONGDECIMAL String
             | UNSIGNEDLONGDECIMAL String
             | CHARACTER Char
             | STRING String
             | IDENTIFIER Name
             | PRIMARYEXPR Expr
             deriving (Eq, Ord, Show)
primary :: Parser Primary
primary = integer'
          +++ character
          +++ string'
          +++ identifier
          +++ do
            e <- parentheses "("  expr ")"
            return (PRIMARYEXPR e)
integer' = octal
           +++ hexadecimal
           +++ unsignedLongDecimal
           +++ longDecimal
           +++ unsignedDecimal
           +++ decimal
octal = do
  o <- token $ string "0o"
  i <-  token $ many1 digit
  return $ DECIMAL (o ++ i)
hexadecimal = do
  h <- token $ string "0x"
  i <-  token $ many1 digit
  return $ DECIMAL (h ++ i)
unsignedLongDecimal = do
  i <-  token $ many1 digit
  token $ string "UL"
  return $ UNSIGNEDLONGDECIMAL i
longDecimal = do
  i <-  token $ many1 digit
  token $ string "L"
  return $ LONGDECIMAL i
unsignedDecimal = do
  i <-  token $ many1 digit
  token $ string "U"
  return $ UNSIGNEDDECIMAL i
decimal = do
  i <-  token $ many1 digit
  return $ DECIMAL i
character = do
  c <-  parentheses "'" ascii "'"
  return $ CHARACTER c
-- string' = do
--   s <-  parentheses "\"" (many str) "\""
--   return $ STRING s
data StringStatus = Normal
                          | InString
string' = do
    cs <- string'' Normal
    return $ STRING cs
    where
        string'' Normal = do
            dmy <-  char '\"'
            cs <- string'' InString
            return cs
        string'' InString = do
            char '\"'
            return []
            +++ do
            c <-  char '\\'
            n <- ascii
            cs <- string'' InString
            return (c:(n:cs))
            +++ do
            c <- str
            cs <- string'' InString
            return (c:cs)
identifier = do
  nm <-  token $ name
  return $ IDENTIFIER nm
-- ŽO€‰‰ŽZŽq expr ? expr : expr10
expr10 :: Parser Expr
expr10 = do
  e9 <- expr9
  do
    token $ string "?"
    e <- expr
    token $ string ":"
    e10 <- expr10
    return (Expr3 TernaryOp e9 e e10)
    +++ return e9
-- “ñ€‰‰ŽZŽq ||
expr9 = do
  e8 <- expr8
  do
    o9 <- operator9
    e9 <- expr9
    return (Expr2 o9 e8 e9)
    +++ return e8
-- “ñ€‰‰ŽZŽq &&
expr8 = do
  e7 <- expr7
  do
    o8 <- operator8
    e8 <- expr8
    return (Expr2 o8 e7 e8)
    +++ return e7
-- “ñ€‰‰ŽZŽq >,<,>=,<=,==,!=
expr7 = do
  e6 <- expr6
  do
    o7 <- operator7
    e7 <- expr7
    return (Expr2 o7 e6 e7)
    +++ return e6
-- “ñ€‰‰ŽZŽq |
expr6 = do
  e5 <- expr5
  do
    o6 <- operator6
    e6 <- expr6
    return (Expr2 o6 e5 e6)
    +++ return e5
-- “ñ€‰‰ŽZŽq ^
expr5 = do
  e4 <- expr4
  do
    o5 <- operator5
    e5 <- expr5
    return (Expr2 o5 e4 e5)
    +++ return e4
-- “ñ€‰‰ŽZŽq &
expr4 = do
  e3 <- expr3
  do
    o4 <- operator4
    e4 <- expr4
    return (Expr2 o4 e3 e4)
    +++ return e3
-- “ñ€‰‰ŽZŽq >>,<<
expr3 :: Parser Expr
expr3 = do
  e2 <- expr2
  do
    o3 <- operator3
    e3 <- expr3
    return (Expr2 o3 e2 e3)
    +++ return e2
-- “ñ€‰‰ŽZŽq +,-
expr2 :: Parser Expr
expr2 = do
  e1 <- expr1
  do
    o2 <- operator2
    e2 <- expr2
    return (Expr2 o2 e1 e2)
    +++ return e1
-- “ñ€‰‰ŽZŽq *,/,%
expr1 = do
  t <- term
  do
    o1 <- operator1
    e1 <- expr1
    return (Expr2 o1 (Expr1 t) e1)
    +++ return (Expr1 t)
stmtblock :: Parser Stmt
stmtblock = do
  blk <- block
  return $ StmtBlock blk
ifstmt :: Parser Stmt
ifstmt = do
  token $ string "if"
  e <- parentheses "(" expr ")"
  thenstmt <- stmt
  do
    token $ string "else"
    elsestmt <- stmt
    return $ IfStmt e thenstmt elsestmt
    +++ return (IfStmt e thenstmt BlankLine)
whilestmt :: Parser Stmt
whilestmt = do
  token $ string "while"
  e <- parentheses "(" expr ")"
  s <- stmt
  return $ WhileStmt e s
dowhilestmt :: Parser Stmt
dowhilestmt = do
  token $ string "do"
  s <- stmt
  token $ string "while"
  e <- parentheses "(" expr ")"
  token $ string ";"
  return $ DoWhileStmt e s
forstmt :: Parser Stmt
forstmt = do
  token $ string "for"
  (e1, e2, e3) <- parentheses "(" 
          (do
            e1 <- expr
            token $ string ";"
            e2 <- expr
            token $ string ";"
            e3 <- expr
            return (e1, e2, e3))
       ")"
  s <- stmt
  return $ ForStmt e1 e2 e3 s
switchstmt :: Parser Stmt
switchstmt = do
  token $ string "switch"
  e <- parentheses "(" expr ")"
  cs <- parentheses "{" 
        (do
          cs' <- many caseclause
          do
            d <- defaultclause
            return (cs' ++ [d])
            +++ return cs') 
         "}"
  return (SwitchStmt e cs)
data Constant = Constant Primary
                deriving (Eq, Ord, Show)
constant :: Parser Constant
constant = do
  p <- primary
  return $ Constant p
type CaseClauses = [CaseClause]
data CaseClause = CaseClause Constant Stmts
                         | DefaultClause Stmts
                deriving (Eq, Ord, Show)
caseclause :: Parser CaseClause
caseclause = do
  token $ string "case"
  c <- constant
  token $ string ":"
  s <- stmtbases
  return $ CaseClause c s
defaultclause :: Parser CaseClause
defaultclause = do
  token $ string "default"
  token $ string ":"
  s <- stmtbases
  return $ DefaultClause s
breakstmt :: Parser Stmt
breakstmt = do
  token $ string "break"
  token $ string ";"
  return BreakStmt
continuestmt :: Parser Stmt
continuestmt = do
  token $ string "continue"
  token $ string ";"
  return ContinueStmt
gotostmt :: Parser Stmt
gotostmt = do
  token $ string "goto"
  nm <- name
  token $ string ";"
  return $ GotoStmt nm
returnstmt :: Parser Stmt
returnstmt = do
  token $ string "return"
  do 
    e <- expr
    return $ ReturnStmt e
    +++ return ReturnVoid
parentheses :: String -> Parser a -> String -> Parser a
parentheses l p r = do
  token $ string l
  elm <- p
  token $ string r
  return elm
type Defvars = [Defvar]
data Defvar = Defvar Storage Typeref Name Value
             deriving (Eq, Ord, Show)
topdefvar :: Parser TopDef
topdefvar = do
  var <- defvar
  return $ TopDefvars var
separator :: String -> Parser a -> Parser a
separator spr p = do
  s <- token $ string spr
  a <- p
  return a
data Storage = NoStorage
                    | Static
               deriving (Eq, Ord, Show)
storage :: Parser Storage
storage = do
    strg <- token $ string "static"
    return Static
    +++ return NoStorage
defnamevalue :: Parser (Name,  Value)
defnamevalue = do
  nm <- name
  do
    eq <- token $ string "="
    val <- value
    return (nm, val)
    +++ return (nm, NoValue)
data Value = NoValue
           | ValueOf Expr
               deriving (Eq, Ord, Show)
value :: Parser Value
value = do
    e <- expr
    return $ ValueOf e
data Defconst = Defconst Typeref Name Expr
                deriving (Eq, Ord, Show)
defconst :: Parser TopDef
defconst = do
  token $ string "const"
  t  <- typeref
  nm <- name
  token $ string "="
  e  <- expr
  token $ string ";"
  return (TopDefconst (Defconst t nm e))
data Defstruct = Defstruct Name MemberList
                 deriving (Eq, Ord, Show)
defstruct :: Parser TopDef
defstruct = do
  token $ string "struct"
  nm <- name
  memlst <- parentheses "{" memberlist "}"
  token $ string ";"
  return $ TopDefstruct (Defstruct nm memlst)
type MemberList = [Slot]
memberlist :: Parser MemberList
memberlist = many slot
data Slot = Slot Typeref Name
                 deriving (Eq, Ord, Show)
slot :: Parser Slot
slot = do
  tp <- typeref
  nm <- name
  token $ string ";"
  return $ Slot tp nm
data Defunion = Defunion Name MemberList
                 deriving (Eq, Ord, Show)
defunion :: Parser TopDef
defunion = do
  token $ string "union"
  nm <- name
  memlst <- parentheses "{" memberlist "}"
  token $ string ";"
  return $ TopDefunion (Defunion nm memlst)
data Typedef = Typedef Typeref Ident
                 deriving (Eq, Ord, Show)
data Typeref = Typeref TyperefBase [Modifier]
               deriving (Eq, Ord, Show)
typeref :: Parser Typeref
typeref = do
  tp <- typerefbase
  m <- many modifier
  return $ Typeref tp m
data Modifier = ArrayLengthNotSpecified
              | ArrayLengthSpecified Int
              | Pointer
              | FunctionPointer ParamTyperefs
                 deriving (Eq, Ord, Show)
modifier :: Parser Modifier
modifier = do
  token $ string "[]"
  return ArrayLengthNotSpecified
  +++ do
  idx <- parentheses "[" integer "]"
  return (ArrayLengthSpecified idx)
  +++ do
  token $ string "*"
  return Pointer
  +++ do
  prmtprfs <- parentheses "(" paramtyperefs ")"
  return (FunctionPointer  prmtprfs)
integer :: Parser Int
integer = do
  i <- token $ many1 digit
  return $ read i
data TyperefBaseCore = CHAR
                 | SHORT
                 | INT
                 | LONG
                 deriving (Eq, Ord, Show)
typerefbasecore :: Parser TyperefBaseCore
typerefbasecore = do
    (token $ string "char") &&& ident
    return CHAR
  +++   do
    (token $ string "short") &&& ident
    return SHORT
  +++   do
    (token $ string "int") &&& ident
    return INT
  +++ do
    (token $ string "long") &&& ident
    return LONG
data TyperefBase = VOID
                 | UNSIGNED TyperefBaseCore
                 | STRUCT Ident
                 | UNION Ident
                 | SIGNED TyperefBaseCore
                 | USERDEF Ident
                 deriving (Eq, Ord, Show)
typerefbase :: Parser TyperefBase
typerefbase = 
  do
    (token $ string "void") &&& ident
    return VOID
  +++ do
    (token $ string "unsigned") &&& ident
    tpcore <- typerefbasecore
    return (UNSIGNED tpcore)
  +++ do
    (token $ string "struct") &&& ident
    idnt <- ident
    return (STRUCT idnt)
  +++ do
    (token $ string "union") &&& ident
    idnt <- ident
    return (UNION idnt)
  +++ do
    tp <- typerefbasecore
    return (SIGNED tp)
  +++ do
    i <- ident
    return (USERDEF i)
data ParamTyperefs = VoidType
            | FixedParamTyperef [ParamTyperef]
            | UnfixedParamTyperef [ParamTyperef]
              deriving (Eq, Ord, Show)
paramtyperefs :: Parser ParamTyperefs
paramtyperefs = 
    do 
      prm <- paramtyperef
      prms <- many (separator "," paramtyperef)
      do
        unfixed <- separator "," (token $ string "...")
        return (UnfixedParamTyperef (prm:prms))
        +++ return (FixedParamTyperef (prm:prms))
    +++
    do
      v <- voidtype
      return v
voidtype :: Parser ParamTyperefs
voidtype = token $ do
  string "void"
  return VoidType
data ParamTyperef = ParamTyperef Typeref
             deriving (Eq, Ord, Show)
paramtyperef :: Parser ParamTyperef
paramtyperef = do
  tp <- typeref
  return $ ParamTyperef tp
type Ident = String
ident :: Parser Ident
ident = token $ do
  alpha <- letter
  alphanums <- many alphanum
  return $ alpha:alphanums
typedef :: Parser TopDef
typedef = do
  token $ string "typedef"
  t <- typeref
  i <- ident
  token $ string ";"
  return $ TopDeftype (Typedef t i)