{-# INCLUDE <pcre.h> #-}
{-# LINE 1 "Regex.hsc" #-}
{-# LANGUAGE CPP,ForeignFunctionInterface #-}
{-# LINE 2 "Regex.hsc" #-}

module Ch17.Regex where

import Foreign
import Foreign.C.Types


{-# LINE 9 "Regex.hsc" #-}

newtype PCREOption = PCREOption { unPCREOption :: CInt}
    deriving (Eq, Show)

caseless :: PCREOption
caseless = PCREOption 1
{-# LINE 15 "Regex.hsc" #-}

dollar_endonly :: PCREOption
dollar_endonly = PCREOption 32
{-# LINE 18 "Regex.hsc" #-}

dotall :: PCREOption
dotall = PCREOption 4