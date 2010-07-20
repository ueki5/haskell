module ElfMagic where
import qualified Data.ByteString.Lazy as L
import GHC.Word

hasElfMagic :: L.ByteString -> Bool
hasElfMagic content = L.take 4 content == elfMagic
  where elfMagic = L.pack [0x7f, 0x45, 0x4c, 0x46]
isElfFile :: FilePath -> IO Bool
isElfFile path = do
  contents <- L.readFile path
  return (hasElfMagic contents)
getElfHead :: FilePath -> IO [GHC.Word.Word8]
getElfHead path = do
  contents <- L.readFile path
  return $ L.unpack $ L.take 4 contents
