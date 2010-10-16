{-# LANGUAGE CPP,ForeignFunctionInterface #-}

module Regex where

import Foreign
import Foreign.C.Types

#include <pcre.h>
-- foreign import ccall "pcre.h sin"
--      c_sin :: CDouble -> CDouble
