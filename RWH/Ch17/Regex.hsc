{-# LANGUAGE CPP,ForeignFunctionInterface #-}
-- when pcre.h is not found
-- hsc2hs Regex.hsc -I /local/include
module Ch17.Regex where

import Foreign
import Foreign.C.Types
import Foreign.C.String

#include <pcre.h>

newtype PCREOption = PCREOption { unPCREOption :: CInt}
    deriving (Eq, Show)

-- caseless :: PCREOption
-- caseless = PCREOption #const PCRE_CASELESS

-- dollar_endonly :: PCREOption
-- dollar_endonly = PCREOption #const PCRE_DOLLAR_ENDONLY

-- dotall :: PCREOption
-- dotall = PCREOption #const PCRE_DOTALL
#{enum PCREOption, PCREOption
  , caseless             = PCRE_CASELESS
  , dollar_endonly       = PCRE_DOLLAR_ENDONLY
  , dotall               = PCRE_DOTALL
  }

combineOptions :: [PCREOption] -> PCREOption
combineOptions = PCREOption . foldr ((.|.) . unPCREOption) 0
type PCRE = ()
foreign import ccall unsafe "pcre.h pcre_compile"
        c_pcre_compile :: CString
                          -> PCREOption
                          -> Ptr CString
                          -> Ptr CInt
                          -> Ptr Word8
                          -> IO (Ptr PCRE)
-- newForeignPtr :: FinalizerPtr a -> Ptr a -> IO (ForeignPtr a)

-- data Regex = Regex !(ForeignPtr PCRE)
--                    !ByteString
--         deriving (Eq, Ord, Show)

data Regex = Regex !(ForeignPtr PCRE)
                   !ByteString
        deriving (Eq, Ord, Show)