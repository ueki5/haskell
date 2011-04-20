module Cflat.Type.Type where
-- import Cflat.Parser.Parser

class TypeChecker a where
  checkType :: a -> Bool