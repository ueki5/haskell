-- 簡易パーサコンビネータ（ふつうのHaskellより）
module MyParsec where
import Char
import List

-- パーサの型
data MyParser tok a = MyParser ([tok] -> Maybe (a,[tok]))

-- モナドインスタンスの定義
instance Monad (MyParser tok) where
  return x = MyParser (\input -> Just (x, input))
  p >>= f = MyParser (\input -> case run p input of
                                  Just (x, input') -> run (f x) input'
                                  Nothing -> Nothing)
type Parser a = MyParser Char a
type LineParser a = MyParser String a

-- パーサを起動する関数
parse :: MyParser tok a -> [tok] -> Either String a
parse p input = case run p input of
                  Just (x, _) -> Right x
                  Nothing -> Left "parse error"
run :: MyParser tok a -> [tok] -> Maybe (a,[tok])
run (MyParser f) input = f input

-- 入力１つをパースするパーサの実装
satisfy :: (tok -> Bool) -> MyParser tok tok
satisfy f = MyParser nextState
  where
    nextState (x:xs) | f x = Just (x, xs)
    nextState _ = Nothing

-- チェック関数を渡して、最初の文字をチェックするパーサを返す関数
firstChar :: (Char -> Bool) -> LineParser String
firstChar f = satisfy (test f)
  where 
    test :: (Char -> Bool) -> [Char] -> Bool
    test f [] = False
    test f (c: _) = f c

-- <|>関数の実装
infixr 1 <|>
(<|>) :: MyParser tok a -> MyParser tok a -> MyParser tok a
p1 <|> p2 = MyParser nextState
  where
    nextState input = case run p1 input of
                        Just x -> Just x
                        Nothing -> run p2 input

-- ０回以上パースする？
many :: MyParser tok a -> MyParser tok [a]
many p = do x <- p
            xs <- many p
            return (x:xs)
      <|> return []

-- １回以上パースする？
many1 :: MyParser tok a -> MyParser tok [a]
many1 p = do x <- p
             xs <- many p
             return (x:xs)

-- ここから少し遊び
-- uekigo1 xs = parse (firstChar (\x -> x == 'c')) xs
-- uekigo2 = parse (firstChar isDigit)
-- uekigo3 xs = parse (firstChar isNumber) xs