-- file: ch01/Words.hs

main = interact wordCount
    where wordCount input = show (length (divWords input)) ++ "\n"
-- ltrim :: String -> String
-- ltrim [] = []
-- ltrim inp@(x:xs) = if isSpace x then ltrim xs
--                                 else inp
div1 :: (String, String) -> (String, String)
div1 ([], r) = ([], r)
div1 ((l:ls), []) = if isSpace l
                    then div1 (ls, [])
                    else div1 (ls, [l])
div1 ((l:ls), r) = if isSpace l 
                   then (ls, r)
                   else div1 (ls, (r++[l]))
divWords :: String -> [String]
divWords [] = []
divWords x = case r of
               [] -> []
               _ -> r:(divWords l) 
         where (l, r) = div1 (x, [])
isSpace :: Char -> Bool
isSpace ' ' = True
isSpace '\t' = True
isSpace '\n' = True
isSpace _ = False