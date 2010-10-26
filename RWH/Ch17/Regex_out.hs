{-# LINE 1 "Regex.hs" #-}
{-# INCLUDE <pcre.h> #-}
{-# LINE 2 "Regex.hs" #-}
{-# LINE 1 "Regex.hsc" #-}
{-# LANGUAGE CPP,ForeignFunctionInterface #-}
{-# LINE 2 "Regex.hsc" #-}
-- when pcre.h is not found
-- hsc2hs Regex.hsc -I /local/include
module Ch17.Regex where

import Foreign
import Foreign.C.Types


{-# LINE 10 "Regex.hsc" #-}

newtype PCREOption = PCREOption { unPCREOption :: CInt}
    deriving (Eq, Show)

-- caseless :: PCREOption
-- caseless = PCREOption #const PCRE_CASELESS

-- dollar_endonly :: PCREOption
-- dollar_endonly = PCREOption #const PCRE_DOLLAR_ENDONLY

-- dotall :: PCREOption
-- dotall = PCREOption #const PCRE_DOTALL
caseless              :: PCREOption
caseless              = PCREOption 1
dollar_endonly        :: PCREOption
dollar_endonly        = PCREOption 32
dotall                :: PCREOption
dotall                = PCREOption 4
