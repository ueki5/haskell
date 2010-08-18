module Ch10.Parse where
import qualified Data.ByteString.Lazy as L
import Data.Int(Int64)
data ParseState = ParseState {
      string::L.ByteString 
     ,offset::Int64
    }
simpleParse :: ParseState -> (a, ParseState)
simpleParse = undefined
betterParse :: ParseState -> (a, ParseState)
betterParse = undefined

newtype Parse a = Parse {
      runParse :: ParseState -> Either String (a, ParseState)
      }
identity :: a -> Parse a
identity a = Parse (\s -> Right (a, s))

-- parse :: Parse a -> ParseState -> Either String (a, ParseState)
parse :: Parse a -> L.ByteString -> Either String a
parse parser initState 
    = case runParse parser (ParseState initState 0) of
        Left err -> Left err
        Right (a, _) -> Right a
