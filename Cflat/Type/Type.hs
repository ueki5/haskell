module Cflat.Type.Type where

rtn = "\n"
class TypeChecker a where
  checkType :: a -> Bool
data CommentStatus = CommentOff
                     | LineOn
                     | RegionOn

data AST = AST [ImportStmt] [TopDef]
                     deriving (Eq, Ord)
instance TypeChecker AST where
  checkType (AST imps defs) = (foldl (&&) True  (map checkType imps))
                              && (foldl (&&) True  (map checkType defs))
data ImportStmt =  Import [Name]
                   deriving (Eq, Ord)
instance TypeChecker ImportStmt where
  checkType (Import names) = True
type Name = String
data TopDef = TopDefvar Defvars
            | TopDefun Defun
            | TopDefconst Defconst
            | TopDefstruct Defstruct
            | TopDefunion Defunion
            | TopDeftype Typedef
             deriving (Eq, Ord)
instance TypeChecker TopDef where
    checkType (TopDefvar defvars) = True
    checkType (TopDefun defun) = True
    checkType (TopDefconst defconst) = True
    checkType (TopDefstruct defstruct) = True
    checkType (TopDefunion defunion) = True
    checkType (TopDeftype typedef) = True
data Defun = Defun  Storage Typeref Name Params Block
             deriving (Eq, Ord, Show)
data Params = Void
            | FixedParam [Param]
            | UnfixedParam [Param]
              deriving (Eq, Ord, Show)
data Param = Param Typeref Name
             deriving (Eq, Ord, Show)
data Block = Block Defvarlist [Stmt]
               deriving (Eq, Ord, Show)
type Defvarlist = [Defvars]
type Defvars = [Defvar]
data Defvar = Defvar Storage Typeref Name Value
             deriving (Eq, Ord, Show)
-- type Stmts = [Stmt]
data Stmt = BlankLine
          | LabeledStmt Name [Stmt]
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
          deriving (Eq, Ord)
instance Show Stmt where
    show BlankLine = "BlankLine" ++ rtn
    show (LabeledStmt name stmts) = "LabeledStmt:" ++ show name ++ rtn
                                    ++ "{" ++ show stmts ++ "}"
    show (StmtExpr e) = "StmtExpr(" ++ show e ++ ")" ++ rtn
    show (StmtBlock block) = "StmtBlock{" ++ show block ++ "}" ++ rtn
    show (IfStmt e s1 s2) = "IfStmt(" ++ show e ++ ")" ++ rtn
                            ++ "then{" ++ show s1 ++ "}" ++ rtn
                            ++ "else{" ++ show s2 ++ "}"
    show (WhileStmt e s) = "WhileStmt(" ++ show e ++ ")" ++ rtn
                           ++ "{" ++ show s ++ "}"
    show (DoWhileStmt e s) = "DoWhileStmt(" ++ show e ++ ")" ++ rtn
                             ++ "{" ++  show s ++ "}"
    show (ForStmt e1 e2 e3 s) = "ForStmt(" ++ show e1 ++ ";" ++ show e2 ++ ";" ++ show e3 ++ ")" ++ rtn
                                ++ "{" ++ show s ++ "}"
    show (SwitchStmt e ss) = "SwitchStmt(" ++ show e ++ ")" ++ rtn
                             ++ show ss
    show (BreakStmt) = "BreakStmt" ++ rtn
    show (ContinueStmt) = "ContinueStmt" ++ rtn
    show (GotoStmt name) = "GotoStmt:" ++ show name ++ rtn
    show (ReturnStmt e) = "ReturnStmt(" ++ show e ++ ")" ++ rtn
    show (ReturnVoid) = "ReturnVoid" ++ rtn
data Expr = ExprAssign Assign
               | ExprOpAssign OpAssign
               | Expr1 Term
               | Expr2 Operator Expr Expr
               | Expr3 Operator Expr Expr Expr
               deriving (Eq, Ord, Show)
data Assign = AssignExpr Term Expr
              deriving (Eq, Ord, Show)
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
data OpAssign = OpAssign Term OpAssignOp Expr
              deriving (Eq, Ord, Show)
data Term = TermCast Typeref Term
          | TermUnary Unary
              deriving (Eq, Ord, Show)
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
data Postfix = PostfixPrimary Primary
             | PostfixComb Primary [Postfix']
             deriving (Eq, Ord)
instance Show Postfix where
    show (PostfixPrimary p) = "PostfixPrimary:" ++ show p
    show (PostfixComb p []) = "PostfixComb:" ++ show p
    show (PostfixComb p (f:fs)) = "PostfixComb:" ++ show p ++ show (f:fs)
data Postfix' = PostfixPlus
              | PostfixMinus
              | RefArray Expr
              | RefMember Name
              | RefByPointer Name
              | FuncCall Args
             deriving (Eq, Ord, Show)
data Args = ArgsExpr [Expr]
            deriving (Eq, Ord, Show)
data BaseUnit = Octal
                           | Decimal
                           | Hexadecimal
                           deriving (Eq, Ord, Show)
data Primary = INTEGER BaseUnit TyperefBase String
             | CHARACTER Char
             | STRING String
             | IDENTIFIER Name
             | PRIMARYEXPR Expr
             deriving (Eq, Ord)
instance Show Primary where 
    show (INTEGER u ty s) = "(" ++ show u ++ ":" ++ show ty ++ ")" ++ s
    show (CHARACTER c) = ('\'':(c:['\'']))
    show (STRING s) = show s
    show (IDENTIFIER name) = name
    show (PRIMARYEXPR e)   = show e
data StringStatus = Normal
                  | InString
data Constant = Constant Primary
                deriving (Eq, Ord, Show)
type CaseClauses = [CaseClause]
data CaseClause = CaseClause Constant [Stmt]
                | DefaultClause [Stmt]
                deriving (Eq, Ord)
instance Show CaseClause where
    show (CaseClause const stmts) = "Case:" ++ show const ++ rtn
                                    ++ show stmts
    show (DefaultClause stmts) = "Default:" ++ rtn
                                    ++ show stmts
data Storage = NoStorage
                    | Static
               deriving (Eq, Ord, Show)
data Value = NoValue
           | ValueOf Expr
               deriving (Eq, Ord, Show)
data Defconst = Defconst Typeref Name Expr
                deriving (Eq, Ord, Show)
data Defstruct = Defstruct Name [Slot]
                 deriving (Eq, Ord)
instance Show Defstruct where
    show (Defstruct name memlist) = "Defstruct:" ++ show name ++ rtn
                                  ++ show memlist
-- type MemberList = [Slot]
data Slot = Slot Typeref Name
            deriving (Eq, Ord)
instance Show Slot where
    show (Slot ty name) = "Slot:" ++ show ty ++ ":" ++ show name ++ rtn
data Defunion = Defunion Name [Slot]
                 deriving (Eq, Ord)
instance Show Defunion where
    show (Defunion name memlist) = "Defunion:" ++ show name ++ rtn
                                  ++ show memlist
data Typedef = Typedef Typeref Ident
                 deriving (Eq, Ord, Show)
data Typeref = Typeref TyperefBase [Modifier]
               deriving (Eq, Ord)
instance Show Typeref where
    show (Typeref b []) = show b 
    show (Typeref b (m:ms)) = show b ++ show (m:ms)
data Modifier = ArrayLengthNotSpecified
              | ArrayLengthSpecified Int
              | Pointer
              | FunctionPointer ParamTyperefs
                 deriving (Eq, Ord, Show)
data TyperefBaseCore = CHAR
                 | SHORT
                 | INT
                 | LONG
                 deriving (Eq, Ord, Show)
type Ident = String
data TyperefBase = VOID
                 | UNSIGNED TyperefBaseCore
                 | SIGNED TyperefBaseCore
                 | STRUCT Ident
                 | UNION Ident
                 | USERDEF Ident
                 deriving (Eq, Ord)
instance Show TyperefBase where
    show VOID = "VOID"
    show (UNSIGNED t) = "U:" ++ show t
    show (SIGNED t)   = show t
    show (STRUCT i)   = "STRCT:" ++ show i
    show (UNION i)    = "UNION:" ++ show i
    show (USERDEF i)  = "USER:" ++ show i
data ParamTyperefs = VoidType
                   | FixedParamTyperef [ParamTyperef]
                   | UnfixedParamTyperef [ParamTyperef]
                     deriving (Eq, Ord)
instance Show ParamTyperefs where
    show VoidType                     = "VoidType"
    show (FixedParamTyperef [])       = "FixedParamTyperef"
    show (FixedParamTyperef (p:ps))   = "FixedParamTyperef:" ++ show (p:ps)
    show (UnfixedParamTyperef [])     = "UnfixedParamTyperef"
    show (UnfixedParamTyperef (p:ps)) = "UnfixedParamTyperef:" ++ show (p:ps)
data ParamTyperef = ParamTyperef Typeref
             deriving (Eq, Ord, Show)
