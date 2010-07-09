-- file: ch01/Chars.hs
main = interact wordCount
    where wordCount input = show (length (filter (not . isSpace) input)) ++ "\n"
isSpace :: Char -> Bool
isSpace ' ' = True
isSpace '\t' = True
isSpace '\n' = True
isSpace _ = False