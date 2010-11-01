{-# INCLUDE <pcre.h> #-}
{-# LINE 1 "Regex.hsc" #-}
{-# LANGUAGE CPP,ForeignFunctionInterface #-}
{-# LINE 2 "Regex.hsc" #-}
-- when pcre.h is not found
-- hsc2hs Regex.hsc -I /local/include
module Ch17.Regex where

import Foreign
import Foreign.C.Types
import Foreign.C.String
-- import Data.ByteString.Char8 hiding (foldr, reverse)
import Data.ByteString.Char8 (ByteString, useAsCString, pack, empty)
import Data.ByteString.Unsafe (unsafeDrop, unsafeTake)
import Data.ByteString.Internal (fromForeignPtr, toForeignPtr)


{-# LINE 15 "Regex.hsc" #-}

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

{-# LINE 32 "Regex.hsc" #-}

newtype PCREInfo = PCREInfo { unPCREInfo :: CInt}
    deriving (Eq, Show)
info_options  :: PCREInfo
info_options  = PCREInfo 0
info_size  :: PCREInfo
info_size  = PCREInfo 1
info_capturecount  :: PCREInfo
info_capturecount  = PCREInfo 2
info_backrefmax  :: PCREInfo
info_backrefmax  = PCREInfo 3
info_firstbyte  :: PCREInfo
info_firstbyte  = PCREInfo 4
info_firstchar  :: PCREInfo
info_firstchar  = PCREInfo 4
info_firsttable  :: PCREInfo
info_firsttable  = PCREInfo 5
info_lastliteral  :: PCREInfo
info_lastliteral  = PCREInfo 6
info_nameentrysize  :: PCREInfo
info_nameentrysize  = PCREInfo 7
info_namecount  :: PCREInfo
info_namecount  = PCREInfo 8
info_nametable  :: PCREInfo
info_nametable  = PCREInfo 9
info_studysize  :: PCREInfo
info_studysize  = PCREInfo 10
info_default_tables  :: PCREInfo
info_default_tables  = PCREInfo 11
info_okpartial  :: PCREInfo
info_okpartial  = PCREInfo 12
info_jchanged  :: PCREInfo
info_jchanged  = PCREInfo 13
info_hascrorlf  :: PCREInfo
info_hascrorlf  = PCREInfo 14
info_minlength  :: PCREInfo
info_minlength  = PCREInfo 15

{-# LINE 54 "Regex.hsc" #-}

combineOptions :: [PCREOption] -> PCREOption
combineOptions = PCREOption . foldr ((.|.) . unPCREOption) 0
combineExecOptions :: [PCREExecOption] -> PCREExecOption
combineExecOptions = foldr (.|.) 0
type PCRE = ()
type PCREExtra = ()
foreign import ccall unsafe "pcre.h pcre_compile"
        c_pcre_compile :: CString
                          -> PCREOption
                          -> Ptr CString
                          -> Ptr CInt
                          -> Ptr Word8
                          -> IO (Ptr PCRE)

data Regex = Regex !(ForeignPtr PCRE)
                   !ByteString
        deriving (Eq, Ord, Show)

compile :: ByteString -> [PCREOption] -> Either String Regex
compile str flags = unsafePerformIO $
  useAsCString str $ \pattern -> do
    alloca $ \errptr       -> do
    alloca $ \erroffset    -> do
        pcre_ptr <- c_pcre_compile pattern (combineOptions flags) errptr erroffset nullPtr
        if pcre_ptr == nullPtr
            then do
                err <- peekCString =<< peek errptr
                return (Left err)
            else do
                reg <- newForeignPtr finalizerFree pcre_ptr -- release with free()
                return (Right (Regex reg str))
-- unsafePerformIO = undefined
-- useAsCString = undefined

type PCREExecOption = CInt
foreign import ccall "pcre.h pcre_exec"
    c_pcre_exec     :: Ptr PCRE
                    -> Ptr PCREExtra
                    -> Ptr Word8
                    -> CInt
                    -> CInt
                    -> PCREExecOption
                    -> Ptr CInt
                    -> CInt
                    -> IO CInt

foreign import ccall "pcre.h pcre_fullinfo"
    c_pcre_fullinfo :: Ptr PCRE
                    -> Ptr PCREExtra
                    -> PCREInfo
                    -> Ptr a
                    -> IO CInt

capturedCount :: Ptr PCRE -> IO Int
capturedCount regex_ptr =
    alloca $ \n_ptr -> do
         c_pcre_fullinfo regex_ptr nullPtr info_capturecount n_ptr
         return . fromIntegral =<< peek (n_ptr :: Ptr CInt)

match :: Regex -> ByteString -> [PCREExecOption] -> Maybe [ByteString]
match (Regex pcre_fp _) subject os = unsafePerformIO $ do
  withForeignPtr pcre_fp $ \pcre_ptr -> do
    n_capt <- capturedCount pcre_ptr
    let ovec_size = (n_capt + 1) * 3
        ovec_bytes = ovec_size * sizeOf (undefined :: CInt)
    allocaBytes ovec_bytes $ \ovec -> do

        let (str_fp, off, len) = toForeignPtr subject
        withForeignPtr str_fp $ \cstr -> do
            r <- c_pcre_exec
                         pcre_ptr
                         nullPtr
                         (cstr `plusPtr` off)
                         (fromIntegral len)
                         0
                         (combineExecOptions os)
                         ovec
                         (fromIntegral ovec_size)
            if r < 0
                then return Nothing
                else let loop n o acc =
                            if n == r
                              then return (Just (reverse acc))
                              else do
                                    i <- peekElemOff ovec o
                                    j <- peekElemOff ovec (o+1)
                                    let s = substring i j subject
                                    loop (n+1) (o+2) (s : acc)
                     in loop 0 0 []

  where
    substring :: CInt -> CInt -> ByteString -> ByteString
    substring x y _ | x == y = empty
    substring a b s = end
        where
            start = unsafeDrop (fromIntegral a) s
            end   = unsafeTake (fromIntegral (b-a)) start
-- fromForeignPtr = undefined
-- toForeignPtr = undefined
-- unsafeDrop = undefined
-- unsafeTake = undefined
