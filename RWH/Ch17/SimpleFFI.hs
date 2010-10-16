{-# LANGUAGE ForeignFunctionInterface #-}

module Ch17.SimpleFFI where

import Foreign
import Foreign.C.Types
import Foreign.C.String
import Foreign.Ptr
import Foreign.Marshal.Array

foreign import ccall "math.h sin"
     c_sin :: CDouble -> CDouble

fastsin :: Double -> Double
fastsin x = realToFrac (c_sin (realToFrac x))
main = mapM_ (print . fastsin) [0/10,1/10 .. 10/10]
-- main = map fastsin [0/10,1/10 .. 10/10]