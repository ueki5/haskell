module Cbc.Parser.Parser where
import Cbc.Parser.ParserConstants
import System.IO
import Control.Monad(liftM)
import Control.Exception (bracket)
import Data.Char

-- import Cbc.Ast
-- import Cbc.Entity
-- import Cbc.Type
-- import Cbc.Asm.Label
-- import Cbc.Utils.ErrorHandler
-- import Cbc.Exception

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

-- 以下Javaコード---------------------------------
    static public AST parseFile(File file, LibraryLoader loader,
                                ErrorHandler errorHandler)
                                throws SyntaxException, FileException {
        return parseFile(file, loader, errorHandler, false);
    }

    // #@@range/parseFile{
    static public AST parseFile(File file, LibraryLoader loader,
                                ErrorHandler errorHandler, boolean debug)
                                throws SyntaxException, FileException {
        return newFileParser(file, loader, errorHandler, debug).parse();
    }
    // #@@}

    static public Declarations parseDeclFile(File file,
                                             LibraryLoader loader,
                                             ErrorHandler errorHandler)
                                             throws SyntaxException,
                                                    FileException {
        return parseDeclFile(file, loader, errorHandler, false);
    }

    static public Declarations parseDeclFile(File file,
                                             LibraryLoader loader,
                                             ErrorHandler errorHandler,
                                             boolean debug)
                                             throws SyntaxException,
                                                    FileException {
        return newFileParser(file, loader, errorHandler, debug).parseDecls();
    }

    // #@@range/newFileParser{
    static final public String SOURCE_ENCODING = "UTF-8";

    static public Parser newFileParser(File file,
                                       LibraryLoader loader,
                                       ErrorHandler errorHandler,
                                       boolean debug)
                                       throws FileException {
        try {
            BufferedReader r =
                new BufferedReader(
                    new InputStreamReader(new FileInputStream(file),
                                          SOURCE_ENCODING));
            return new Parser(r, file.getPath(), loader, errorHandler, debug);
        }
        catch (FileNotFoundException ex) {
            throw new FileException(ex.getMessage());
        }
        catch (UnsupportedEncodingException ex) {
            throw new Error("UTF-8 is not supported??: " + ex.getMessage());
        }
    }
    // #@@}

    // #@@range/instance_members{
    private String sourceName;
    private LibraryLoader loader;
    private ErrorHandler errorHandler;
    private Set<String> knownTypedefs;
    // #@@}

    // #@@range/ctor1{
    public Parser(Reader s, String name,
                  LibraryLoader loader, ErrorHandler errorHandler) {
        this(s, name, loader, errorHandler, false);
    }
    // #@@}

    // #@@range/ctor2{
    public Parser(Reader s, String name, LibraryLoader loader,
                  ErrorHandler errorHandler, boolean debug) {
        this(s);
        this.sourceName = name;
        this.loader = loader;
        this.errorHandler = errorHandler;
        this.knownTypedefs = new HashSet<String>();
        if (debug) {
            enable_tracing();
        }
        else {
            disable_tracing();
        }
    }
    // #@@}

    // #@@range/parse{
    public AST parse() throws SyntaxException {
        try {
            return compilation_unit();
        }
        catch (TokenMgrError err) {
            throw new SyntaxException(err.getMessage());
        }
        catch (ParseException ex) {
            throw new SyntaxException(ex.getMessage());
        }
        catch (LookaheadSuccess err) {
            throw new SyntaxException("syntax error");
        }
    }
    // #@@}

    public Declarations parseDecls() throws SyntaxException {
        try {
            return declaration_file();
        }
        catch (TokenMgrError ex) {
            throw new SyntaxException(ex.getMessage());
        }
        catch (ParseException ex) {
            throw new SyntaxException(ex.getMessage());
        }
    }

    private void addKnownTypedefs(List<TypedefNode> typedefs) {
        for (TypedefNode n : typedefs) {
            addType(n.name());
        }
    }

    private void addType(String name) {
        knownTypedefs.add(name);
    }

    private boolean isType(String name) {
        return knownTypedefs.contains(name);
    }

    // #@@range/newReader{
    private IntegerLiteralNode integerNode(Location loc, String image) {
        long i = integerValue(image);
        if (image.endsWith("UL")) {
            return new IntegerLiteralNode(loc, IntegerTypeRef.ulongRef(), i);
        }
        else if (image.endsWith("L")) {
            return new IntegerLiteralNode(loc, IntegerTypeRef.longRef(), i);
        }
        else if (image.endsWith("U")) {
            return new IntegerLiteralNode(loc, IntegerTypeRef.uintRef(), i);
        }
        else {
            return new IntegerLiteralNode(loc, IntegerTypeRef.intRef(), i);
        }
    }
    // #@@}

    // #@@range/integerValue{
    private long integerValue(String image) {
        String s = image.replaceFirst("[UL]+", "");
        if (s.startsWith("0x") || s.startsWith("0X")) {
            return Long.parseLong(s.substring(2), 16);
        }
        else if (s.startsWith("0") && !s.equals("0")) {
            return Long.parseLong(s.substring(1), 8);
        }
        else {
            return Long.parseLong(s, 10);
        }
    }
    // #@@}

    // #@@range/characterCode{
    private long characterCode(String image) throws ParseException {
        String s = stringValue(image);
        if (s.length() != 1) {
            throw new Error("must not happen: character length > 1");
        }
        return (long)s.charAt(0);
    }
    // #@@}

    // #@@range/stringValue{
    private String stringValue(String _image) throws ParseException {
        int pos = 0;
        int idx;
        StringBuffer buf = new StringBuffer();
        String image = _image.substring(1, _image.length() - 1);

        while ((idx = image.indexOf("\\", pos)) >= 0) {
            buf.append(image.substring(pos, idx));
            if (image.length() >= idx + 4
                    && Character.isDigit(image.charAt(idx+1))
                    && Character.isDigit(image.charAt(idx+2))
                    && Character.isDigit(image.charAt(idx+3))) {
                buf.append(unescapeOctal(image.substring(idx+1, idx+4)));
                pos = idx + 4;
            }
            else {
                buf.append(unescapeSeq(image.charAt(idx+1)));
                pos = idx + 2;
            }
        }
        if (pos < image.length()) {
            buf.append(image.substring(pos, image.length()));
        }
        return buf.toString();
    }
    // #@@}

    private TypeRef size_t() {
        return IntegerTypeRef.ulongRef();
    }

    // #@@range/unescapeOctal{
    private static final int charMax = 255;

    private char unescapeOctal(String digits) throws ParseException {
        int i = Integer.parseInt(digits, 8);
        if (i > charMax) {
            throw new ParseException(
                "octal character sequence too big: \\" + digits);
        }
        return (char)i;
    }
    // #@@}

    // #@@range/unescapeSeq{
    private static final char bell = 7;
    private static final char backspace = 8;
    private static final char escape = 27;
    private static final char vt = 11;

    private char unescapeSeq(char c) throws ParseException {
        switch (c) {
        case '0': return '\0';
        case '"': return '"';
        case '\'': return '\'';
        case 'a': return bell;
        case 'b': return backspace;
        case 'e': return escape;
        case 'f': return '\f';
        case 'n': return '\n';
        case 'r': return '\r';
        case 't': return '\t';
        case 'v': return vt;
        default:
            throw new 
ParseException("unknown escape sequence: \"\\" + c);
        }
    }
    // #@@}

    // #@@range/location{
    protected Location location(Token t) {
        return new Location(sourceName, t);
    }
    // #@@}

PARSER_END(Parser)

/*
 * Lexical Rules
 */

// linear-white-spaces
// #@@range/lex_spaces{
SPECIAL_TOKEN: { <SPACES: ([" ", "\t", "\n", "\r", "\f"])+> }
// #@@}

// block comment
// #@@range/lex_block_comment{
MORE: { <"/*"> : IN_BLOCK_COMMENT }
<IN_BLOCK_COMMENT> MORE: { <~[]> }
<IN_BLOCK_COMMENT> SPECIAL_TOKEN: { <BLOCK_COMMENT: "*/"> : DEFAULT }
// #@@}

// line comment
// #@@range/lex_line_comment{
SPECIAL_TOKEN: {
    <LINE_COMMENT: "//" (~["\n", "\r"])* ("\n" | "\r\n" | "\r")?>
}
// #@@}

// reserved words
// #@@range/lex_reswords{
TOKEN: {
      <VOID     : "void">
    | <CHAR     : "char">
    | <SHORT    : "short">
    | <INT      : "int">
    | <LONG     : "long">
    | <STRUCT   : "struct">
    | <UNION    : "union">
    | <ENUM     : "enum">
    | <STATIC   : "static">
    | <EXTERN   : "extern">
    | <CONST    : "const">
    | <SIGNED   : "signed">
    | <UNSIGNED : "unsigned">
    | <IF       : "if">
    | <ELSE     : "else">
    | <SWITCH   : "switch">
    | <CASE     : "case">
    | <DEFAULT_ : "default">
    | <WHILE    : "while">
    | <DO       : "do">
    | <FOR      : "for">
    | <RETURN   : "return">
    | <BREAK    : "break">
    | <CONTINUE : "continue">
    | <GOTO     : "goto">
    | <TYPEDEF  : "typedef">
    | <IMPORT   : "import">
    | <SIZEOF   : "sizeof">
}
// #@@}

// identifier
// #@@range/lex_ident{
TOKEN: {
    <IDENTIFIER: ["a"-"z", "A"-"Z", "_"] (["a"-"z", "A"-"Z", "_", "0"-"9"])*>
}
// #@@}

// integer literals
// #@@range/lex_integer{
TOKEN: {
    <INTEGER: ["1"-"9"] (["0"-"9"])* ("U")? ("L")?
            | "0" ["x", "X"] (["0"-"9", "a"-"f", "A"-"F"])+ ("U")? ("L")?
            | "0" (["0"-"7"])* ("U")? ("L")?
            >
}
// #@@}

// character literal
// #@@range/lex_char{
MORE: { <"'"> : IN_CHARACTER }                         // rule1
<IN_CHARACTER> MORE: {
      <~["'", "\\", "\n", "\r"]> : CHARACTER_TERM      // rule2
    | <"\\" (["0"-"7"]){3}>      : CHARACTER_TERM      // rule3
    | <"\\" ~[]>                 : CHARACTER_TERM      // rule4
}
<CHARACTER_TERM> TOKEN: { <CHARACTER: "'"> : DEFAULT } // rule5
// #@@}

// string literal
// #@@range/lex_string{
MORE: { <"\""> : IN_STRING }                           // rule1
<IN_STRING> MORE: {
      <(~["\"", "\\", "\n", "\r"])+>                   // rule2
    | <"\\" (["0"-"7"]){3}>                            // rule3
    | <"\\" ~[]>                                       // rule4
}
<IN_STRING> TOKEN: { <STRING: "\""> : DEFAULT }        // rule5
// #@@}

/*
 *  Grammar
 */

// #@@range/compilation_unit{
AST compilation_unit():
{
    Token t;
    Declarations impdecls, decls;
}
{
        {
            t = getToken(1);
        }
    impdecls=import_stmts() decls=top_defs() <EOF>
        {
            decls.add(impdecls);
            return new AST(location(t), decls);
        }
}
// #@@}

// #@@range/declaration_file{
Declarations declaration_file():
{
    Declarations impdecls, decls = new Declarations();
    UndefinedFunction funcdecl;
    UndefinedVariable vardecl;
    Constant defconst;
    StructNode defstruct;
    UnionNode defunion;
    TypedefNode typedef;
}
{
    impdecls=import_stmts()
        {
            decls.add(impdecls);
        }
    ( LOOKAHEAD(<EXTERN> typeref() <IDENTIFIER> "(")
      funcdecl=funcdecl()   { decls.addFuncdecl(funcdecl); }
    | vardecl=vardecl()     { decls.addVardecl(vardecl); }
    | defconst=defconst()   { decls.addConstant(defconst); }
    | defstruct=defstruct() { decls.addDefstruct(defstruct); }
    | defunion=defunion()   { decls.addDefunion(defunion); }
    | typedef=typedef()     { decls.addTypedef(typedef); }
    )*
    <EOF>
        {
            return decls;
        }
}
// #@@}

// #@@range/import_stmts{
Declarations import_stmts():
{
    String libid;
    Declarations impdecls = new Declarations();
}
{
    (libid=import_stmt()
        {
            try {
                Declarations decls = loader.loadLibrary(libid, errorHandler);
                if (decls != null) {
                    impdecls.add(decls);
                    addKnownTypedefs(decls.typedefs());
                }
            }
            catch (CompileException ex) {
                throw new ParseException(ex.getMessage());
            }
        }
    )*
        {
            return impdecls;
        }
}
// #@@}

// #@@range/import_stmt{
String import_stmt():
{
    StringBuffer buf = new StringBuffer();
    String n;
}
{
    <IMPORT> n=name()   { buf.append(n); }
    ("." n=name()       { buf.append("."); buf.append(n); } )*
    ";"
        {
            return buf.toString();
        }
}
// #@@}

// #@@range/top_defs{
Declarations top_defs():
{
    Declarations decls = new Declarations();
    DefinedFunction defun;
    List<DefinedVariable> defvars;
    Constant defconst;
    StructNode defstruct;
    UnionNode defunion;
    TypedefNode typedef;
}
{
    ( LOOKAHEAD(storage() typeref() <IDENTIFIER> "(")
      defun=defun()         { decls.addDefun(defun); }
    | LOOKAHEAD(3)
      defvars=defvars()     { decls.addDefvars(defvars); }
    | defconst=defconst()   { decls.addConstant(defconst); }
    | defstruct=defstruct() { decls.addDefstruct(defstruct); }
    | defunion=defunion()   { decls.addDefunion(defunion); }
    | typedef=typedef()     { decls.addTypedef(typedef); }
    )*
        {
            return decls;
        }
}
// #@@}

// #@@range/defvars{
List<DefinedVariable> defvars():
{
    List<DefinedVariable> defs = new ArrayList<DefinedVariable>();
    boolean priv;
    TypeNode type;
    String name;
    ExprNode init = null;
}
{
    priv=storage() type=type() name=name() ["=" init=expr()]
        {
            defs.add(new DefinedVariable(priv, type, name, init));
            init = null;
        }
    ( "," name=name() ["=" init=expr()]
        {
            defs.add(new DefinedVariable(priv, type, name, init));
            init = null;
        }
    )* ";"
        {
            return defs;
        }
}
// #@@}

// #@@range/defconst{
Constant defconst():
{
    TypeNode type;
    String name;
    ExprNode value;
}
{
    <CONST> type=type() name=name() "=" value=expr() ";"
        {
            return new Constant(type, name, value);
        }
}
// #@@}

// #@@range/defun{
DefinedFunction defun():
{
    boolean priv;
    TypeRef ret;
    String n;
    Params ps;
    BlockNode body;
}
{
    priv=storage() ret=typeref() n=name() "(" ps=params() ")" body=block()
        {
            TypeRef t = new FunctionTypeRef(ret, ps.parametersTypeRef());
            return new DefinedFunction(priv, new TypeNode(t), n, ps, body);
        }
}
// #@@}

// #@@range/storage{
boolean storage():
{ Token t = null; }
{
    [t=<STATIC>] { return (t == null ? false : true); }
}
// #@@}

// #@@range/storage{
Params params():
{
    Token t;
    Params params;
}
{
      LOOKAHEAD(<VOID> ")")
      t=<VOID>
        {
            return new Params(location(t), new ArrayList<Parameter>());
        }
    | params=fixedparams()
            ["," "..." { params.acceptVarargs(); }]
        {
            return params;
        }
}
// #@@}

// #@@range/fixedparams{
Params fixedparams():
{
    List<Parameter> params = new ArrayList<Parameter>();
    Parameter param, param1;
}
{
    param1=param() { params.add(param1); }
    ( LOOKAHEAD(2) "," param=param() { params.add(param); } )*
        {
            return new Params(param1.location(), params);
        }
}
// #@@}

// #@@range/param{
Parameter param():
{
    TypeNode t;
    String n;
}
{
    t=type() n=name() { return new Parameter(t, n); }
}
// #@@}

// #@@range/block{
BlockNode block():
{
    Token t;
    List<DefinedVariable> vars;
    List<StmtNode> stmts;
}
{
    t="{" vars=defvar_list() stmts=stmts() "}"
        {
            return new BlockNode(location(t), vars, stmts);
        }
}
// #@@}


// #@@range/defvar_list{
List<DefinedVariable> defvar_list():
{
    List<DefinedVariable> result = new ArrayList<DefinedVariable>();
    List<DefinedVariable> vars;
}
{
    ( vars=defvars() { result.addAll(vars); } )*
        {
            return result;
        }
}
// #@@}

// #@@range/defstruct{
StructNode defstruct():
{
    Token t;
    String n;
    List<Slot> membs;
}
{
    t=<STRUCT> n=name() membs=member_list() ";"
        {
            return new StructNode(location(t), new StructTypeRef(n), n, membs);
        }
}
// #@@}

UnionNode defunion():
{
    Token t;
    String n;
    List<Slot> membs;
}
{
    t=<UNION> n=name() membs=member_list() ";"
        {
            return new UnionNode(location(t), new UnionTypeRef(n), n, membs);
        }
}

// #@@range/member_list{
List<Slot> member_list():
{
    List<Slot> membs = new ArrayList<Slot>();
    Slot s;
}
{
    "{" (s=slot() ";" { membs.add(s); })* "}"
        {
            return membs;
        }
}
// #@@}

// #@@range/slot{
Slot slot():
{
    TypeNode t;
    String n;
}
{
    t=type() n=name() { return new Slot(t, n); }
}
// #@@}

// #@@range/funcdecl{
UndefinedFunction funcdecl():
{
    TypeRef ret;
    String n;
    Params ps;
}
{
    <EXTERN> ret=typeref() n=name() "(" ps=params() ")" ";"
        {
            TypeRef t = new FunctionTypeRef(ret, ps.parametersTypeRef());
            return new UndefinedFunction(new TypeNode(t), n, ps);
        }
}
// #@@}

UndefinedVariable vardecl():
{
    TypeNode t;
    String n;
}
{
    <EXTERN> t=type() n=name() ";"
        {
            return new UndefinedVariable(t, n);
        }
}

// #@@range/type{
TypeNode type():
{ TypeRef ref; }
{
    ref=typeref() { return new TypeNode(ref); }
}
// #@@}

// #@@range/typeref{
TypeRef typeref():
{
    TypeRef ref;
    Token t;
    ParamTypeRefs params;
}
{
    ref=typeref_base()
    ( LOOKAHEAD(2)
      "[" "]"
        {
            ref = new ArrayTypeRef(ref);
        }
    | "[" t=<INTEGER> "]"
        {
            ref = new ArrayTypeRef(ref, integerValue(t.image));
        }
    | "*"
        {
            ref = new PointerTypeRef(ref);
        }
    | "(" params=param_typerefs() ")"
        {
            ref = new FunctionTypeRef(ref, params);
        }
    )*
        {
            return ref;
        }
}
// #@@}

// #@@range/param_typerefs{
ParamTypeRefs param_typerefs():
{ ParamTypeRefs params; }
{
      LOOKAHEAD(<VOID> ")")
      <VOID>
        {
            return new ParamTypeRefs(new ArrayList<TypeRef>());
        }
    | params=fixedparam_typerefs()
          [ "," "..."   { params.acceptVarargs(); }]
        {
            return params;
        }
}
// #@@}

// #@@range/fixedparam_typerefs{
ParamTypeRefs fixedparam_typerefs():
{
    List<TypeRef> refs = new ArrayList<TypeRef>();
    TypeRef ref;
}
{
    ref=typeref() { refs.add(ref); }
    ( LOOKAHEAD(2) "," ref=typeref() { refs.add(ref); } )*
        {
            return new ParamTypeRefs(refs);
        }
}
// #@@}

// #@@range/typeref_base{
TypeRef typeref_base():
{
    Token t, name;
}
{
      t=<VOID>          { return new VoidTypeRef(location(t)); }
    | t=<CHAR>          { return IntegerTypeRef.charRef(location(t)); }
    | t=<SHORT>         { return IntegerTypeRef.shortRef(location(t)); }
    | t=<INT>           { return IntegerTypeRef.intRef(location(t)); }
    | t=<LONG>          { return IntegerTypeRef.longRef(location(t)); }
    | LOOKAHEAD(2) t=<UNSIGNED> <CHAR>
        { return IntegerTypeRef.ucharRef(location(t)); }
    | LOOKAHEAD(2) t=<UNSIGNED> <SHORT>
        { return IntegerTypeRef.ushortRef(location(t)); }
    | LOOKAHEAD(2) t=<UNSIGNED> <INT>
        { return IntegerTypeRef.uintRef(location(t)); }
    | t=<UNSIGNED> <LONG>
        { return IntegerTypeRef.ulongRef(location(t)); }
    | t=<STRUCT> name=<IDENTIFIER>
        { return new StructTypeRef(location(t), name.image); }
    | t=<UNION> name=<IDENTIFIER>
        { return new UnionTypeRef(location(t), name.image); }
    | LOOKAHEAD({isType(getToken(1).image)}) name=<IDENTIFIER>
        { return new UserTypeRef(location(name), name.image); }
}
// #@@}

// #@@range/typedef{
TypedefNode typedef():
{
    Token t;
    TypeRef ref;
    Token newname;
}
{
    t=<TYPEDEF> ref=typeref() newname=<IDENTIFIER> ";"
        {
            addType(newname.image);
            return new TypedefNode(location(t), ref, newname.image);
        }
}
// #@@}

// #@@range/stmts{
List<StmtNode> stmts():
{
    List<StmtNode> ss = new ArrayList<StmtNode>();
    StmtNode s;
}
{
    (s=stmt() { if (s != null) ss.add(s); })*
        {
            return ss;
        }
}
// #@@}

// #@@range/stmt{
StmtNode stmt():
{
    StmtNode n = null;
    ExprNode e = null;
}
{
    ( ";"
    | LOOKAHEAD(2) n=labeled_stmt()
    | e=expr() ";" { n = new ExprStmtNode(e.location(), e); }
    | n=block()
    | n=if_stmt()
    | n=while_stmt()
    | n=dowhile_stmt()
    | n=for_stmt()
    | n=switch_stmt()
    | n=break_stmt()
    | n=continue_stmt()
    | n=goto_stmt()
    | n=return_stmt()
    )
        {
            return n;
        }
}
// #@@}

LabelNode labeled_stmt():
{
    Token t;
    StmtNode n;
}
{
    t=<IDENTIFIER> ":" n=stmt()
        {
            return new LabelNode(location(t), t.image, n);
        }
}

// #@@range/if_stmt{
IfNode if_stmt():
{
    Token t;
    ExprNode cond;
    StmtNode thenBody, elseBody = null;
}
{
    t=<IF> "(" cond=expr() ")" thenBody=stmt()
            [LOOKAHEAD(1) <ELSE> elseBody=stmt()]
        {
            return new IfNode(location(t), cond, thenBody, elseBody);
        }
}
// #@@}

// #@@range/while_stmt{
WhileNode while_stmt():
{
    Token t;
    ExprNode cond;
    StmtNode body;
}
{
    t=<WHILE> "(" cond=expr() ")" body=stmt()
        {
            return new WhileNode(location(t), cond, body);
        }
}
// #@@}

DoWhileNode dowhile_stmt():
{
    Token t;
    ExprNode cond;
    StmtNode body;
}
{
    t=<DO> body=stmt() <WHILE> "(" cond=expr() ")" ";"
        {
            return new DoWhileNode(location(t), body, cond);
        }
}

ForNode for_stmt():
{
    Token t;
    ExprNode init = null, cond = null, incr = null;
    StmtNode body;
}
{
    t=<FOR> "(" [init=expr()] ";"
              [cond=expr()] ";"
              [incr=expr()] ")" body=stmt()
        {
            return new ForNode(location(t), init, cond, incr, body);
        }
}

SwitchNode switch_stmt():
{
    Token t;
    ExprNode cond;
    List<CaseNode> bodies;
}
{
    t=<SWITCH> "(" cond=expr() ")" "{" bodies=case_clauses() "}"
        {
            return new SwitchNode(location(t), cond, bodies);
        }
}

List<CaseNode> case_clauses():
{
    List<CaseNode> clauses = new ArrayList<CaseNode>();
    CaseNode n;
}
{
    (n=case_clause() { clauses.add(n); })*
            [n=default_clause() { clauses.add(n); }]
        {
            return clauses;
        }
}

CaseNode case_clause():
{
    List<ExprNode> values;
    BlockNode body;
}
{
    values=cases() body=case_body()
        {
            return new CaseNode(body.location(), values, body);
        }
}

List<ExprNode> cases():
{
    List<ExprNode> values = new ArrayList<ExprNode>();
    ExprNode n;
}
{
    (<CASE> n=primary() ":" { values.add(n); })+
        {
            return values;
        }
}

CaseNode default_clause():
{ BlockNode body; }
{
    <DEFAULT_> ":" body=case_body()
        {
            return new CaseNode(body.location(), new ArrayList<ExprNode>(), body);
        }
}

BlockNode case_body():
{
    LinkedList<StmtNode> stmts = new LinkedList<StmtNode>();
    StmtNode s;
}
{
    (s=stmt() { if (s != null) stmts.add(s); })+
        {
            // last stmt of case clause must be break stmt.
            if (! (stmts.getLast() instanceof BreakNode)) {
                throw new ParseException(
                  "missing break statement at the last of case clause");
            }
            return new BlockNode(stmts.get(0).location(),
                                 new ArrayList<DefinedVariable>(),
                                 stmts);
        }
}

GotoNode goto_stmt():
{ Token t, name; }
{
    t=<GOTO> name=<IDENTIFIER> ";"
        {
            return new GotoNode(location(t), name.image);
        }
}

// #@@range/break_stmt{
BreakNode break_stmt():
{ Token t; }
{
    t=<BREAK> ";" { return new BreakNode(location(t)); }
}
// #@@}

ContinueNode continue_stmt():
{ Token t; }
{
    t=<CONTINUE> ";" { return new ContinueNode(location(t)); }
}

ReturnNode return_stmt():
{
    Token t;
    ExprNode expr;
}
{
      LOOKAHEAD(2) t=<RETURN> ";" { return new ReturnNode(location(t), null); }
    | t=<RETURN> expr=expr() ";"  { return new ReturnNode(location(t), expr); }
}

// #@@range/expr{
ExprNode expr():
{
    ExprNode lhs, rhs, expr;
    String op;
}
{
      LOOKAHEAD(term() "=")
      lhs=term() "=" rhs=expr()
        {
            return new AssignNode(lhs, rhs);
        }
    | LOOKAHEAD(term() opassign_op())
      lhs=term() op=opassign_op() rhs=expr()
        {
            return new OpAssignNode(lhs, op, rhs);
        }
    | expr=expr10()
        {
            return expr;
        }
}
// #@@}

// #@@range/opassign_op{
String opassign_op(): {}
{
    ( "+="  { return "+"; }
    | "-="  { return "-"; }
    | "*="  { return "*"; }
    | "/="  { return "/"; }
    | "%="  { return "%"; }
    | "&="  { return "&"; }
    | "|="  { return "|"; }
    | "^="  { return "^"; }
    | "<<=" { return "<<"; }
    | ">>=" { return ">>"; }
    )
}
// #@@}

// #@@range/expr10{
ExprNode expr10():
{ ExprNode c, t, e; }
{
    c=expr9() ["?" t=expr() ":" e=expr10()
                    { return new CondExprNode(c, t, e); }]
        {
            return c;
        }
}
// #@@}

// #@@range/expr9{
ExprNode expr9():
{ ExprNode l, r; }
{
    l=expr8() ("||" r=expr8() { l = new LogicalOrNode(l, r); })*
        {
            return l;
        }
}
// #@@}

// #@@range/expr8{
ExprNode expr8():
{ ExprNode l, r; }
{
    l=expr7() ("&&" r=expr7() { l = new LogicalAndNode(l, r); })*
        {
            return l;
        }
}
// #@@}

// #@@range/expr7{
ExprNode expr7():
{ ExprNode l, r; }
{
    l=expr6() ( ">"  r=expr6() { l = new BinaryOpNode(l, ">", r); }
              | "<"  r=expr6() { l = new BinaryOpNode(l, "<", r); }
              | ">=" r=expr6() { l = new BinaryOpNode(l, ">=", r); }
              | "<=" r=expr6() { l = new BinaryOpNode(l, "<=", r); }
              | "==" r=expr6() { l = new BinaryOpNode(l, "==", r); }
              | "!=" r=expr6() { l = new BinaryOpNode(l, "!=", r); } )*
        {
            return l;
        }
}
// #@@}

// #@@range/expr6{
ExprNode expr6():
{ ExprNode l, r; }
{
    l=expr5() ("|" r=expr5() { l = new BinaryOpNode(l, "|", r); })*
        {
            return l;
        }
}
// #@@}

// #@@range/expr5{
ExprNode expr5():
{ ExprNode l, r; }
{
    l=expr4() ("^" r=expr4() { l = new BinaryOpNode(l, "^", r); })*
        {
            return l;
        }
}
// #@@}

// #@@range/expr4{
ExprNode expr4():
{ ExprNode l, r; }
{
    l=expr3() ("&" r=expr3() { l = new BinaryOpNode(l, "&", r); })*
        {
            return l;
        }
}
// #@@}

// #@@range/expr3{
ExprNode expr3():
{ ExprNode l, r; }
{
    l=expr2() ( ">>" r=expr2() { l = new BinaryOpNode(l, ">>", r); }
              | "<<" r=expr2() { l = new BinaryOpNode(l, "<<", r); }
              )*
        {
            return l;
        }
}
// #@@}

// #@@range/expr2{
ExprNode expr2():
{ ExprNode l, r; }
{
    l=expr1() ( "+" r=expr1() { l = new BinaryOpNode(l, "+", r); }
              | "-" r=expr1() { l = new BinaryOpNode(l, "-", r); }
              )*
        {
            return l;
        }
}
// #@@}

// #@@range/expr1{
ExprNode expr1():
{ ExprNode l, r; }
{
    l=term() ( "*" r=term() { l = new BinaryOpNode(l, "*", r); }
             | "/" r=term() { l = new BinaryOpNode(l, "/", r); }
             | "%" r=term() { l = new BinaryOpNode(l, "%", r); }
             )*
        {
            return l;
        }
}
// #@@}

// #@@range/term{
ExprNode term():
{
    TypeNode t;
    ExprNode n;
}
{
      LOOKAHEAD("(" type())
      "(" t=type() ")" n=term()     { return new CastNode(t, n); }
    | n=unary()                     { return n; }
}
// #@@}

// #@@range/unary{
ExprNode unary():
{
    ExprNode n;
    TypeNode t;
}
{
      "++" n=unary()    { return new PrefixOpNode("++", n); }
    | "--" n=unary()    { return new PrefixOpNode("--", n); }
    | "+" n=term()      { return new UnaryOpNode("+", n); }
    | "-" n=term()      { return new UnaryOpNode("-", n); }
    | "!" n=term()      { return new UnaryOpNode("!", n); }
    | "~" n=term()      { return new UnaryOpNode("~", n); }
    | "*" n=term()      { return new DereferenceNode(n); }
    | "&" n=term()      { return new AddressNode(n); }
    | LOOKAHEAD(3) <SIZEOF> "(" t=type() ")"
        {
            return new SizeofTypeNode(t, size_t());
        }
    | <SIZEOF> n=unary()
        {
            return new SizeofExprNode(n, size_t());
        }
    | n=postfix()       { return n; }
}
// #@@}

// #@@range/postfix{
ExprNode postfix():
{
    ExprNode expr, idx;
    String memb;
    List<ExprNode> args;
}
{
    expr=primary()
    ( "++"                  { expr = new SuffixOpNode("++", expr); }
    | "--"                  { expr = new SuffixOpNode("--", expr); }
    | "[" idx=expr() "]"    { expr = new ArefNode(expr, idx); }
    | "." memb=name()       { expr = new MemberNode(expr, memb); }
    | "->" memb=name()      { expr = new PtrMemberNode(expr, memb); }
    | "(" args=args() ")"   { expr = new FuncallNode(expr, args); }
    )*
        {
            return expr;
        }
}
// #@@}

// #@@range/name{
String name():
{ Token t; }
{
    t=<IDENTIFIER> { return t.image; }
}
// #@@}

// #@@range/args{
List<ExprNode> args():
{
    List<ExprNode> args = new ArrayList<ExprNode>();
    ExprNode arg;
}
{
    [ arg=expr() { args.add(arg); }
      ("," arg=expr() { args.add(arg); })* ]
        {
            return args;
        }
}
// #@@}

// #@@range/primary{
ExprNode primary():
{
    Token t;
    ExprNode n;
}
{
      t=<INTEGER>
        {
            return integerNode(location(t), t.image);
        }
    | t=<CHARACTER>
        {
            return new IntegerLiteralNode(location(t),
                                          IntegerTypeRef.charRef(),
                                          characterCode(t.image));
        }
    | t=<STRING>
        {
            return new StringLiteralNode(location(t),
                new PointerTypeRef(IntegerTypeRef.charRef()),
                stringValue(t.image));
        }
    | t=<IDENTIFIER>
        {
            return new VariableNode(location(t), t.image);
        }
    | "(" n=expr() ")"
        {
            return n;
        }
}
// #@@}
