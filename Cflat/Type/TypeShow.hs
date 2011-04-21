module Cflat.Type.TypeShow where
import Cflat.Type.Type
instance Show AST where
    show (AST imp defs) = "AST(" ++ rtn
                          ++ show imp
                          ++ ","
                          ++ show defs
                          ++ ")"
instance Show ImportStmt where
    show (Import names) = "Import(" ++ show names ++ ")" ++ rtn
instance Show TopDef where
    show (TopDefvar defvars) = "TopDefvar(" ++ show defvars ++ ")" ++ rtn
    show (TopDefun defun) = "TopDefun(" ++ show defun ++ ")" ++ rtn
    show (TopDefconst defconst) = "TopDefConst(" ++ show defconst ++ ")" ++ rtn
    show (TopDefstruct defstruct) = "Defstruct(" ++ show defstruct ++ ")" ++ rtn
    show (TopDefunion defunion) = "TopDefunion(" ++ show defunion ++ ")" ++ rtn
    show (TopDeftype typedef) = "TopDeftype(" ++ show typedef ++ ")" ++ rtn
