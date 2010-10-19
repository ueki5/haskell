{-# LANGUAGE CPP,ForeignFunctionInterface #-}

module Ch17.Regex where

import Foreign
import Foreign.C.Types

#include <pcre.h>

newType PCREOption = PCREOption { unPCREOption :: CInt}
    deriving (Eq, Show)

caseless :: PCREOption
caseless = PCREOption #const PCRE_CASELESS

dollar_endonly :: PCREOption
dollar_endonly = PCREOption #const PCRE_DOLLAR_ENDONLY

dotall :: PCREOption
dotall = PCREOption #const PCRE_DOTALL