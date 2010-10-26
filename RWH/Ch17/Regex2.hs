{-# LANGUAGE ForeignFunctionInterface #-}
module Ch17.Regex2 where
import Ch17.Regex 
import Foreign
import Foreign.C.Types
import Foreign.C.String
-- import Foreign.Ptr
import Foreign.Marshal.Array

combineOptions :: [PCREOption] -> PCREOption
combineOptions = PCREOption . foldr ((.|.) . unPCREOption) 0
-- pcre *pcre_compile(const char *pattern,
--                    int options,
--                    const char **errptr,
--                    int *erroffset,
--                    const unsigned char *tableptr);
type PCRE = ()
foreign import ccall unsafe "pcre.h pcre_compile"
        c_pcre_compile :: CString
                          -> PCREOption
                          -> Ptr CString
                          -> Ptr CInt
                          -> Ptr Word8
                          -> IO (Ptr PCRE)
