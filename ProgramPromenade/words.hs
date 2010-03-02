import Data.Char

{- 
Prelude �� lines �� words

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

-- ���ڤ��Ƚ��Ҹ��Ϳ�����Ǥ� words

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

-- CSV (comma separated values) ������ʸ�����ʬ�򤹤�

csv :: String -> [String]
csv = map trim . words2 (','==)

trim :: String -> String                      -- ʸ��������������κ��
trim = reverse . dropWhile isSpace . reverse

-- �������ȶ������ڤ�ʸ���ˤ���ʬ�򤹤�

wordsInSentence :: String -> [String]
wordsInSentence = words2 (`elem` " ,.!?")

-- �ǡ������γ��Ϥ�ɽ��ʸ���Ƚ�λ��ɽ��ʸ���Ȥ���(�Υꥹ��)��ѥ�᡼����

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

-- Apache(HTTP ������)���Ԥ�ʬ��

accessLog :: String -> [String]
accessLog = words3 [('[',']'),('"','"')] isSpace
