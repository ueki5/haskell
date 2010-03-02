--タイプ指定
type Parser a = String -> [(a, String)]
--必ず成功するパーサ
ok::a -> Parser a
ok v = \inp -> [(v, inp)]
--必ず失敗するパーサ
failure::Parser a
failure = \inp -> []
--１つだけ文字列を消費するパーサ
item::Parser Char
item = \inp -> case inp of
                 [] -> []
                 (x:xs) -> [(x,xs)]
--パーサ適用関数
parse::Parser a -> String -> [(a, String)]
parse p inp = p inp

-- sosite::Parser a -> (a -> Parser b) -> Parser b
-- p `sosite` f = \inp -> case parse p inp of
--                 [] -> []
--                 [(v, out)] -> parse (f v) out
--myparser1::String -> Parser a
--myparser1 inp = (sosite item item) inp
p::Parser (Char, Char)
p = do x <- item
       item
       y <- item
       return (x, y)