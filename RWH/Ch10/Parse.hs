module Ch10.Parse where
import qualified Data.ByteString.Lazy.Char8 as L8
import qualified Data.ByteString.Lazy as L
import Data.Word
import Data.Int(Int64)
import Control.Applicative ( (<$>) )
import Data.Char
import Ch10.PNM

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
-- parse :: Parse a -> ParseState -> Either String (a, ParseState)
-- parse parser initState 
--     = runParse parser initState
parse :: Parse a -> L8.ByteString -> Either String a
parse parser initState 
    = case runParse parser (ParseState initState 0) of
        Left err -> Left err
        Right (a, _) -> Right a
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
instance Functor Parse where
    fmap f parser = parser ==> \result ->
                    identity (f result)

w2c :: Word8 -> Char
w2c = chr . fromIntegral

parseChar :: Parse Char
parseChar = w2c <$> parseByte

peakByte :: Parse (Maybe Word8)
-- peakByte = (fmap fst . L.uncons . string) <$> getState
peakByte = fmap (fmap fst . L.uncons . string) getState

peakChar :: Parse (Maybe Char)
-- peakChar = fmap w2c <$> peakByte
peakChar = fmap (fmap w2c) peakByte

parseWhile :: (Word8 -> Bool) -> Parse [Word8]
parseWhile p = (fmap (fmap p) peakByte) ==> \mp ->
                   if mp == (Just True)
                   then parseByte ==> \b -> 
                       (b:) <$> parseWhile p
                   else identity []
parseWhileVerbose :: (Word8 -> Bool) -> Parse [Word8]
parseWhileVerbose p = peakByte ==> \mc ->
                      case mc of
                        Nothing -> identity []
                        Just c | p c ->
                                   parseByte ==> \b ->
                                   parseWhileVerbose p ==> \bs ->
                                   identity (b:bs)
                               | otherwise ->
                                   identity []
parseRawPGM = 
    parseWhileWith w2c notWhite ==> \header -> skipSpaces ==>&
    assert (header == "P5") "invalid raw header" ==>&
    parseNat ==> \width -> skipSpaces ==>&
    parseNat ==> \height -> skipSpaces ==>&
    parseNat ==> \maxGrey -> 
    parseByte ==>&
    parseBytes (width * height) ==> \bitmap ->
    identity (Greymap width height maxGrey bitmap)
  where notWhite = (`notElem` " \r\t\n")
parseWhileWith :: (Word8 -> a) -> (a -> Bool) -> Parse [a]
parseWhileWith w2a p = fmap w2a <$> parseWhile (p . w2a)

parseNat :: Parse Int
parseNat = parseWhileWith w2c isDigit ==> \digits ->
               if null digits
               then bail "no more input"
               else let n = read digits
                    in if n < 0
                       then bail "integer overflow"
                       else identity n

(==>&) :: Parse a -> Parse b -> Parse b
(==>&) a b = a ==> \_ -> b

skipSpaces :: Parse ()
skipSpaces = parseWhileWith w2c isSpace ==>& identity ()

assert :: Bool -> String -> Parse ()
assert True _ = identity ()
assert False err = bail err

parseBytes :: Int -> Parse L.ByteString
parseBytes n = getState ==> \st ->
               let n' = fromIntegral n
                   (h, t) = L.splitAt n' (string st)
                   st' = ParseState {string = h, offset = (offset st) + L.length h}
               in putState st' ==>&
                  assert (L.length h == n') "end of input" ==>&
                  identity h