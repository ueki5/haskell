module Ch10.PNM where
import qualified Data.ByteString.Lazy.Char8 as L8
import qualified Data.ByteString.Lazy as L
import Data.Char (isSpace)
data Greymap = Greymap {
      greyWidth ::Int
    , greyHeight :: Int
    , greyMax :: Int
    , greyData :: L.ByteString
    } deriving (Eq)
matchHeader :: L.ByteString -> L.ByteString -> Maybe L.ByteString
matchHeader = undefined
getNat :: L.ByteString -> Maybe (Int, L.ByteString)
getNat = undefined
getBytes :: Int -> L.ByteString -> Maybe (L.ByteString, L.ByteString)
getBytes = undefined

parseP5 s = 
    case matchHeader (L8.pack "P5") s of
      Nothing -> Nothing
      Just s1 -> 
          case getNat s1 of
            Nothing -> Nothing
            Just (width, s2) ->
                case getNat s2 of
                  Nothing -> Nothing
                  Just (height, s3) ->
                      case getNat s3 of
                        Nothing -> Nothing
                        Just (maxGrey, s4)
                            | maxGrey > 255 -> Nothing
                            | otherwise -> 
                                case getBytes 1 s4 of
                                  Nothing -> Nothing
                                  Just (_, s5) -> 
                                    case getBytes (width * height) s5 of
                                      Nothing -> Nothing
                                      Just (bitmap, s6) -> 
                                          Just (Greymap width height maxGrey, bitmap ,s6)