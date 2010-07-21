import qualified Data.ByteString.Lazy.Char8 as L

closing = readPrice . (!!4) . L.split ','
readPrice :: L.ByteString -> Maybe Int
readPrice str =
    case L.readInt str of
      Nothing             -> Nothing
      Just (dollars,rest) -> 
          case L.readInt (L.tail rest) of
            Nothing           -> Nothing
            Just (cents,more) ->
                Just (dollars * 100 + cents)
-- highestClose :: L.ByteString -> Maybe Int
highestClose = maximum . (Nothing:) . map closing . L.lines
-- lowestClose :: L.ByteString -> Maybe Int
lowestClose = minimum . map closing . drop 1 . L.lines
-- allClose :: L.ByteString -> [Maybe Int]
allClose = map closing . drop 1 . L.lines
highestCloseFrom = closeFromWith highestClose
lowestCloseFrom = closeFromWith lowestClose
-- closeFromWith :: (L.ByteString -> Maybe Int) -> FilePath -> IO ()
closeFromWith func path = do
  contents <- L.readFile path
  print (func contents)
-- allCloseFrom :: FilePath -> IO ()
allCloseFrom path = do
  contents <- L.readFile path
  print (allClose contents)
