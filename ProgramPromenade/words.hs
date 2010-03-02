import Data.Char

{- 
Prelude の lines と words

lines :: String -> [String]
lines "" = []
lines s  = let (l,s') = break ('\n'==) s
           in l : case s' of
                    []      -> []
                    (_:s'') -> lines s''

words :: String -> [String]
words s  = case dropWhile Char.isSpace s of
             "" -> []
             s' -> w : words s''
                   where (w, s'') = break Char.isSpace s'
-}

-- 区切り子判定述語を与える版の words

words2 :: (Char -> Bool) -> String -> [String]
words2 p s  = case dropWhile isSpace s of
               "" -> []
               s' -> w : words2 p s''
                     where 
                       (w, s'') = case break p s' of
                                    (v,_:s'') -> (v,s'')
                                    vs        -> vs

{-
words = words2 isSpace
-}

-- CSV (comma separated values) 形式の文字列を分解する

csv :: String -> [String]
csv = map trim . words2 (','==)

trim :: String -> String                      -- 文字列後尾の白空白の削除
trim = reverse . dropWhile isSpace . reverse

-- 句読点と空白を区切り文字にして分解する

wordsInSentence :: String -> [String]
wordsInSentence = words2 (`elem` " ,.!?")

-- データ塊の開始を表す文字と終了を表す文字との組(のリスト)をパラメータ化

words3 :: [(Char, Char)] -> (Char -> Bool) -> String -> [String]
words3 ps p s 
 = case dropWhile isSpace s of
     ""         -> []
     ccs@(c:cs) -> case lookup c ps of
                     Nothing -> case break p ccs of
                                  (v,"")    -> [trim v]
                                  (v,_:cs') -> trim v : words3 ps p cs'
                     Just cl -> case break (cl==) cs of
                                  (v,"")    -> error ("closing '"++cl:"' not foud")
                                  (v,_:cs') -> v : words3 ps p cs'

-- Apache(HTTP サーバ)ログ行の分解

accessLog :: String -> [String]
accessLog = words3 [('[',']'),('"','"')] isSpace
