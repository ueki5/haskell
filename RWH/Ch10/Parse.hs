module Ch10.Parse where
import qualified Data.ByteString.Lazy.Char8 as L8
import qualified Data.ByteString.Lazy as L
import Data.Word

import Data.Int(Int64)
-- import Ch10.PNM
data ParseState = ParseState {
      string::L.ByteString 
     ,offset::Int64
    } deriving (Show)
simpleParse :: ParseState -> (a, ParseState)
simpleParse = undefined
betterParse :: ParseState -> (a, ParseState)
betterParse = undefined

newtype Parse a = Parse {
      runParse :: ParseState -> Either String (a, ParseState)
      }
identity :: a -> Parse a
identity a = Parse (\s -> Right (a, s))

-- parse :: Parse a -> L.ByteString -> Either String a
-- parse parser initState 
--     = case runParse parser (ParseState initState 0) of
--         Left err -> Left err
--         Right (a, _) -> Right a
parse :: Parse a -> ParseState -> Either String (a, ParseState)
parse parser initState 
    = runParse parser initState
modifyOffset :: ParseState -> Int64 -> ParseState
-- modifyOffset (ParseState str ofs) n = ParseState str (ofs + n)
modifyOffset initState newoffset = initState { offset = newoffset}
addOffset :: ParseState -> Int64 -> ParseState
-- modifyOffset (ParseState str ofs) n = ParseState str (ofs + n)
-- addOffset initState addoffset = ParseState (string initState) ((offset initState) + addoffset)
addOffset initState addoffset = initState {offset =  (offset initState) + addoffset}
parseByte :: Parse Word8
parseByte =
    getState ==> \initState ->
    case L.uncons (string initState) of
      Nothing -> bail "no more input"
      Just (byte, remainder) -> 
          putState newState ==> \_ ->
          identity byte
        where newState = initState {string = remainder, offset = newOffset}
              newOffset = offset initState + 1
getState :: Parse ParseState
getState = Parse (\s -> Right (s, s))
putState :: ParseState -> Parse ()
putState s = Parse (\_ -> Right ((), s))
bail :: String -> Parse a
bail err = Parse $ \s -> Left $ "byte offset" ++ show (offset s) ++ ": " ++ err
(==>) :: Parse a -> (a -> Parse b) -> Parse b
(==>) firstParser secondParser = Parse chainedParser
    where chainedParser initState = 
             case runParse firstParser initState of
                Left errMessage -> Left errMessage
                Right (firstResult, newState) -> runParse (secondParser firstResult) newState
uekiTest cs = runParse (parseByte ==> 
                        \_ -> parseByte ==>
                        \_ -> parseByte ==>
                        \_ -> parseByte ==>
                        \_ -> parseByte
                       )
                       (ParseState (L8.pack cs) 0)
