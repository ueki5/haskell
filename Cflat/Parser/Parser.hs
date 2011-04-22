module Cflat.Parser.Parser where
import System.IO
import Control.Monad(liftM)
import Control.Exception (bracket)
import Data.Char
import Cflat.Type.Type

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

failure :: Parser a
failure = Parser $ \inp -> Nothing
item :: Parser Char
item = Parser $ \inp -> case inp of
  [] -> Nothing
  (c:cs) -> Just (c, cs)
-- どちらかが成功した場合に採用
(+++) :: Parser a -> Parser a -> Parser a
(+++) p q = Parser $ \inp -> case parser p inp of
                      Nothing -> parser q inp
                      Just (v, out) -> Just (v, out)
-- 双方が成功し、かつ消費した文字数が同じだった場合に採用
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
octal :: Parser Char
octal = sat isOctal
  where 
    isOctal :: Char -> Bool
    isOctal '0' = True
    isOctal '1' = True
    isOctal '2' = True
    isOctal '3' = True
    isOctal '4' = True
    isOctal '5' = True
    isOctal '6' = True
    isOctal '7' = True
    isOctal _ = False
hexadecimal :: Parser Char
hexadecimal = sat isHexadecimal
  where 
    isHexadecimal :: Char -> Bool
    isHexadecimal 'a' = True
    isHexadecimal 'b' = True
    isHexadecimal 'c' = True
    isHexadecimal 'd' = True
    isHexadecimal 'e' = True
    isHexadecimal 'f' = True
    isHexadecimal 'A' = True
    isHexadecimal 'B' = True
    isHexadecimal 'C' = True
    isHexadecimal 'D' = True
    isHexadecimal 'E' = True
    isHexadecimal 'F' = True
    isHexadecimal c = isDigit c
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

-- 予約語
_void = token $ string "void"
_char = token $ string "char"
_short = token $ string "short"
_int = token $ string "int"
_long = token $ string "long"
_struct = token $ string "struct"
_union = token $ string "union"
_enum = token $ string "enum"
_static = token $ string "static"
_extern = token $ string "extern"
_const = token $ string "const"
_signed = token $ string "signed"
_unsigned = token $ string "unsigned"
_if = token $ string "if"
_else = token $ string "else"
_switch = token $ string "switch"
_case = token $ string "case"
_default = token $ string "default"
_while = token $ string "while"
_do = token $ string "do"
_for = token $ string "for"
_return = token $ string "return"
_break = token $ string "break"
_continue = token $ string "continue"
_goto = token $ string "goto"
_typedef = token $ string "typedef"
_import = token $ string "import"
_sizeof = token $ string "sizeof"

compilationUnit :: Parser AST
compilationUnit = do
  imp_stmts <- importStmts
  top_defs <- topDefs
  return $ AST imp_stmts top_defs
importStmts ::  Parser [ImportStmt]
importStmts =  many importStmt
importStmt :: Parser ImportStmt
importStmt = do
  imp <- _import
  nms <- names
  semc <- token $ string ";"
  return $ Import nms
names :: Parser [Name]
names = do
  nm <- name
  nms <- many $ separator "." name
  return (nm:nms)
name :: Parser Name
name = ident

topDefs :: Parser [TopDef]
topDefs = many topDef
topDef :: Parser TopDef
topDef = liftM TopDefvar defvar
         +++ defun
         +++ defconst
         +++ defstruct
         +++ defunion
         +++ typedef
defun :: Parser TopDef
defun = do
  strg <- storage
  tp <- typeref
  nm <- name
  prms <- parentheses "(" params ")"
  blk <- block
  return (TopDefun  (Defun strg tp nm prms blk))
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
      v <- _void
      return Void
-- void = token $ do
--   string "void"
--   return Void
param = do
  tp <- typeref
  nm <- name
  return $ Param tp nm
block = parentheses 
        "{" (do
              lst <- defvarlist
              ss <- stmts
              return $ Block lst ss) "}"
defvarlist = liftM concat (many defvar)
defvar = do
  strg <- storage
  tp <- typeref
  valnm <- defnamevalue
  valnms <- many (separator "," defnamevalue)
  token $ string ";"
  return  (map (\(nm,  val) -> (Defvar strg tp nm val)) (valnm:valnms))
stmtbases :: Parser [Stmt]
stmtbases = do
  ss <- many stmtbase
  return ss
stmts :: Parser [Stmt]
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
assign :: Parser Expr
assign = do
  t <- term
  token $ string "="
  e <- expr
  return (ExprAssign (AssignExpr t e))
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
opassign :: Parser Expr
opassign = do
  t <- term
  o <- opassignop
  e <- expr
  return (ExprOpAssign (OpAssign t o e))
term :: Parser Term
term = do
  tp <- parentheses "(" typeref ")"
  tm <- term
  return $ TermCast tp tm
  +++ do
    u <- unary
    return $ TermUnary u
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
     _sizeof
     tp <- parentheses "(" typeref ")"
     return $ Sizeoftype tp
   +++ do
     _sizeof
     u <- unary
     return $ Sizeofunary u
   +++ do
     p <- postfix
     return $ UnaryPostfix  p
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
-- args :: Parser Args
args :: Parser [Expr]
args = do
  e <- expr
  es <- many (separator "," expr)
  return $ (e:es)
  +++ return []
primary :: Parser Primary
primary = integer'
          +++ character
          +++ string'
          +++ identifier
          +++ do
            e <- parentheses "("  expr ")"
            return (PRIMARYEXPR e)
integer' = do
    u <- baseUnit
    i <-  token $ many1 
          (case u of
                Octal ->  octal
                Decimal ->  digit
                Hexadecimal ->  hexadecimal)
    t <- typerefbaseSuffix
    return $ INTEGER u t i
baseUnit :: Parser BaseUnit
baseUnit = do
    token $ string "0o"
    return Octal
    +++ do
    token $ string "0x"
    return Hexadecimal
    +++ return Decimal
typerefbaseSuffix = do
    token $ string "UL"
    return (UNSIGNED LONG)
    +++ do
    token $ string "L"
    return (SIGNED LONG)
    +++ do
    token $ string "U"
    return $ (UNSIGNED INT)
    +++ return (SIGNED INT)
character = do
  c <-  parentheses "'" ascii "'"
  return $ CHARACTER c
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
binExpr :: Parser Expr -> Parser Operator -> Parser Expr -> Parser Expr
binExpr p1 op p2 = do
  e1 <- p1
  do
    o <- op
    e2 <- p2
    return (Expr2 o e1 e2)
    +++ return e1
-- 三項演算子 expr ? expr : expr10
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
-- 二項演算子 ||
expr9 = binExpr expr8 operator9 expr9
-- 二項演算子 &&
expr8 = binExpr expr7 operator8 expr8
-- 二項演算子 >,<,>=,<=,==,!=
expr7 = binExpr expr6 operator7 expr7
-- 二項演算子 |
expr6 = binExpr expr5 operator6 expr6
-- 二項演算子 ^
expr5 = binExpr expr4 operator5 expr5
-- 二項演算子 &
expr4 = binExpr expr3 operator4 expr4
-- 二項演算子 >>,<<
expr3 = binExpr expr2 operator3 expr3
-- 二項演算子 +,-
expr2 = binExpr expr1 operator2 expr2
-- 二項演算子 *,/,%
expr1 = binExpr (liftM Expr1 term) operator1 expr1
stmtblock :: Parser Stmt
stmtblock = do
  blk <- block
  return $ StmtBlock blk
ifstmt :: Parser Stmt
ifstmt = do
  _if
  e <- parentheses "(" expr ")"
  thenstmt <- stmt
  do
    _else
    elsestmt <- stmt
    return $ IfStmt e thenstmt elsestmt
    +++ return (IfStmt e thenstmt BlankLine)
whilestmt :: Parser Stmt
whilestmt = do
  _while
  e <- parentheses "(" expr ")"
  s <- stmt
  return $ WhileStmt e s
dowhilestmt :: Parser Stmt
dowhilestmt = do
  _do
  s <- stmt
  _while
  e <- parentheses "(" expr ")"
  token $ string ";"
  return $ DoWhileStmt e s
forstmt :: Parser Stmt
forstmt = do
  _for
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
  _switch
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
constant :: Parser Constant
constant = do
  p <- primary
  return $ Constant p
caseclause :: Parser CaseClause
caseclause = do
  _case
  c <- constant
  token $ string ":"
  s <- stmtbases
  return $ CaseClause c s
defaultclause :: Parser CaseClause
defaultclause = do
  _default
  token $ string ":"
  s <- stmtbases
  return $ DefaultClause s
breakstmt :: Parser Stmt
breakstmt = do
  _break
  token $ string ";"
  return BreakStmt
continuestmt :: Parser Stmt
continuestmt = do
  _continue
  token $ string ";"
  return ContinueStmt
gotostmt :: Parser Stmt
gotostmt = do
  _goto
  nm <- name
  token $ string ";"
  return $ GotoStmt nm
returnstmt :: Parser Stmt
returnstmt = do
  _return
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
separator :: String -> Parser a -> Parser a
separator spr p = do
  s <- token $ string spr
  a <- p
  return a
storage :: Parser Storage
storage = do
    _static
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
value :: Parser Value
value = do
    e <- expr
    return $ ValueOf e
defconst :: Parser TopDef
defconst = do
  _const
  t  <- typeref
  nm <- name
  token $ string "="
  e  <- expr
  token $ string ";"
  return (TopDefconst (Defconst t nm e))
defstruct :: Parser TopDef
defstruct = do
  _struct
  nm <- name
  memlst <- parentheses "{" memberlist "}"
  token $ string ";"
  return $ TopDefstruct (Defstruct nm memlst)
memberlist :: Parser [Slot]
memberlist = many slot
slot :: Parser Slot
slot = do
  tp <- typeref
  nm <- name
  token $ string ";"
  return $ Slot tp nm
defunion :: Parser TopDef
defunion = do
  _union
  nm <- name
  memlst <- parentheses "{" memberlist "}"
  token $ string ";"
  return $ TopDefunion (Defunion nm memlst)
typeref :: Parser Typeref
typeref = do
  tp <- typerefbase
  m <- many modifier
  return $ Typeref tp m
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
typerefbasecore :: Parser TyperefBaseCore
typerefbasecore = do
    ident &&& _char
    return CHAR
  +++   do
    ident &&& _short
    return SHORT
  +++   do
    ident &&& _int
    return INT
  +++ do
    ident &&& _long
    return LONG
typerefbase :: Parser TyperefBase
typerefbase = 
  do
    ident &&& _void
    return VOID
  +++ do
    ident &&& _unsigned
    tpcore <- typerefbasecore
    return (UNSIGNED tpcore)
  +++ do
    tp <- typerefbasecore
    return (SIGNED tp)
  +++ do
    ident &&& _struct
    idnt <- ident
    return (STRUCT idnt)
  +++ do
    ident &&& _union
    idnt <- ident
    return (UNION idnt)
  +++ do
    i <- ident
    return (USERDEF i)
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
voidtype = do
  _void
  return VoidType
-- paramtyperef :: Parser ParamTyperef
paramtyperef :: Parser Typeref
paramtyperef = typeref
ident :: Parser Ident
ident = token $ do
  alpha <- letter
  alphanums <- many alphanum
  return $ alpha:alphanums
typedef :: Parser TopDef
typedef = do
  _typedef
  t <- typeref
  i <- ident
  token $ string ";"
  return $ TopDeftype (Typedef t i)