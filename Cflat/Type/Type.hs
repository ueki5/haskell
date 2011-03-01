module Cflat.Type.Type where

data CompilationUnit = CompilationUnit (ImportStmts, TopDefs)
              deriving (Eq, Ord, Show)
data ImportStmts =  ImportStmt
                    | Cons ImportStmt ImportStmts
                 deriving (Eq, Ord, Show)
data ImportStmt = Import Names
                 deriving (Eq, Ord, Show)
data Names = Name
             | Dot Name Names
              deriving (Eq, Ord, Show)
type Name = String
data TopDefs = TopDefs
              deriving (Eq, Ord, Show)
