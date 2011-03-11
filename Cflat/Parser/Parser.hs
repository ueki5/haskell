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
(+++) p q = Parser $ \inp -> case parser p inp of
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
            | TopDeftype Typedef
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
  "{" 
  (do
     lst <- defvarlist
     ss <- stmts
     return $ Block lst ss)
  "}"
type Defvarlist = [Defvars]
defvarlist = many defvar
defvar = do
  tp <- typeref
  valnm <- defnamevalue
  valnms <- many (separator "," defnamevalue)
  semc <- token $ string ";"
  return (map (\(nm,  val) -> (Defvar NoStorage tp nm val)) (valnm:valnms))
type Stmts = [Stmt]
data Stmt = BlankLine
          | LabeledStmt Name Stmts
          | StmtExpr Expr
          | Block2 Stmts
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
               | E10 Expr10
               | E9 Expr9
               | E8 Expr8
               | E7 Expr7
               | E6 Expr6
               | E5 Expr5
               | E4 Expr4
               | E3 Expr3
               | E2 Expr2
               | E1 Expr1
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
  +++ block2
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
         return (E10 e10)
data Assign = AssignExpr Term Expr
              deriving (Eq, Ord, Show)
assign :: Parser Expr
assign = do
  t <- term
  token $ string "="
  e <- expr
  return (ExprAssign (AssignExpr t e))
data Operator = Pararell
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
operator :: Parser Operator
operator = do
  token $ string "||"
  return Pararell
  +++ do
  token $ string "&&"
  return DoubleAmpasand
  +++ do
  token $ string ">"
  return GreaterThan
  +++ do
  token $ string "<"
  return LessThan
  +++ do
  token $ string ">="
  return GreaterOrEqual
  +++ do
  token $ string "<="
  return LessOrEqual
  +++ do
  token $ string "=="
  return Equal
  +++ do
  token $ string "!="
  return NotEqual
  +++ do
  token $ string "|"
  return Virticalbar
  +++ do
  token $ string "^"
  return Circumflex
  +++ do
  token $ string "&"
  return Ampersand
  +++ do
  token $ string "<<"
  return ShiftLeft
  +++ do
  token $ string ">>"
  return ShiftRight
  +++ do
  token $ string "+"
  return Plus
  +++ do
  token $ string "*"
  return Minus
  +++ do
  token $ string "*"
  return Mult
  +++ do
  token $ string "/"
  return Div
  +++ do
  token $ string "%"
  return Mod
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
data Term = Term
              deriving (Eq, Ord, Show)
term :: Parser Term
term = undefined
data Expr10 = Expr10Single Expr9
            | Expr10Comp Expr9 Expr Expr10
              deriving (Eq, Ord, Show)
-- ŽO€‰‰ŽZŽq expr ? expr : expr10
expr10 :: Parser Expr10
expr10 = do
  e9 <- expr9
  do
    token $ string "?"
    e <- expr
    token $ string ":"
    e10 <- expr10
    return (Expr10Comp e9 e e10)
    +++ return (Expr10Single e9)
-- “ñ€‰‰ŽZŽq ||
data Expr9 = Expr9
             deriving (Eq, Ord, Show)
expr9 = do
  el <- expr8
  do
   er <- many expr9'
  where 
    expr9' = do
          o <- operator
          e <- expr8
          return 
-- “ñ€‰‰ŽZŽq &&
data Expr8 = Expr8
             deriving (Eq, Ord, Show)
-- “ñ€‰‰ŽZŽq >,<,>=,<=,==,!=
expr8 = undefined
data Expr7 = Expr7
             deriving (Eq, Ord, Show)
-- “ñ€‰‰ŽZŽq |
expr7 = undefined
data Expr6 = Expr6
             deriving (Eq, Ord, Show)
expr6 = undefined
-- “ñ€‰‰ŽZŽq ^
data Expr5 = Expr5
             deriving (Eq, Ord, Show)
expr5 = undefined
-- “ñ€‰‰ŽZŽq &
data Expr4 = Expr4
             deriving (Eq, Ord, Show)
expr4 = undefined
-- “ñ€‰‰ŽZŽq >>,<<
data Expr3 = Expr3
             deriving (Eq, Ord, Show)
expr3 = undefined
-- “ñ€‰‰ŽZŽq +,-
data Expr2 = Expr2
             deriving (Eq, Ord, Show)
expr2 = undefined
-- “ñ€‰‰ŽZŽq *,/,%
data Expr1 = Expr1
             deriving (Eq, Ord, Show)
expr1 = undefined
block2 :: Parser Stmt
block2 = do
  ss <- parentheses "{" stmts "}"
  return $ Block2 ss
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
            token $ string ";"
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
data Constant = Constant TyperefBaseCore
                deriving (Eq, Ord, Show)
constant :: Parser Constant
constant = undefined
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
defvars :: Parser TopDef
defvars = do
  strg <- storage
  tp <- typeref
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
           | Value String
               deriving (Eq, Ord, Show)
value :: Parser Value
value = token $ do
   val <- many1 alphanum
   return (Value val)
defconst :: a
defconst = undefined
data Defstruct = Defstruct Name MemberList
                 deriving (Eq, Ord, Show)
defstruct :: Parser TopDef
defstruct = do
  token $ string "struct"
  nm <- name
  memlst <- memberlist
  semc <- token $ string ";"
  return $ TopDefstruct (Defstruct nm memlst)
type MemberList = [Slot]
memberlist :: Parser MemberList
memberlist = many1 slot
data Slot = Slot Typeref Name
                 deriving (Eq, Ord, Show)
slot :: Parser Slot
slot = do
  tp <- typeref
  nm <- name
  semc <- token $ string ";"
  return $ Slot tp nm
data Defunion = Defunion Name MemberList
                 deriving (Eq, Ord, Show)
defunion :: Parser TopDef
defunion = do
  token $ string "union"
  nm <- name
  memlst <- memberlist
  semc <- token $ string ";"
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
    token $ string "char"
    return CHAR
  +++   do
   token $ string "short"
   return SHORT
  +++   do
    token $ string "int"
    return INT
  +++ do
    token $ string "long"
    return LONG
data TyperefBase = VOID
                 | UNSIGNED TyperefBaseCore
                 | STRUCT Ident
                 | UNION Ident
                 -- | Ident
                 deriving (Eq, Ord, Show)
typerefbase :: Parser TyperefBase
typerefbase = 
  do
    token $ string "void"
    return VOID
  +++ do
    token $ string "unsigned"
    tpcore <- typerefbasecore
    return (UNSIGNED tpcore)
  +++ do
    token $ string "struct"
    idnt <- ident
    return (STRUCT idnt)
  +++ do
    token $ string "union"
    idnt <- ident
    return (UNION idnt)
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
  alf <- letter
  alfnums <- many alphanum
  return $ alf:alfnums
typedef :: Parser TopDef
typedef = do
  t <- typeref
  i <- ident
  return $ TopDeftype (Typedef t i)