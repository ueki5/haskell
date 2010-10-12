{-# LANGUAGE ForeignFunctionInterface #-}

module Ch17.SimpleFFI where

import Foreign
import Foreign.C.Types
import Foreign.C.String
import Foreign.Ptr
import Foreign.Marshal.Array

foreign import ccall "math.h sin"
     c_sin :: CDouble -> CDouble