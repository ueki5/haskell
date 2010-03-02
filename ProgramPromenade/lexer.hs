import Data.Char
import Control.Monad.State

type Lexeme  = String
type Lexer l = State String l

runLexer :: Lexer l -> String -> (l, String)
runLexer = runState

lexResult :: Lexer l -> String -> l
lexResult = evalState

lexWord :: Lexer Lexeme
lexWord = State $ break isSpace . dropWhile isSpace 

lexSkipChar :: Lexer ()                         -- 入力文字列の先頭の1文字を読み捨てる
lexSkipChar = modify $ \ s -> if null s then s else tail s

-- modify は Control.Monad.State で定義されている．
-- modify :: (MonadState s m) => (s -> s) -> m ()
-- modify f = do { s <- get; return (f s) }

lexUntilSep :: Char -> Lexer Lexeme          -- 指定した区切り文字の直前までを読み込む
lexUntilSep sep = State $ break (sep==)

lexBracketed :: Char -> Char -> Lexer Lexeme -- 指定した文字に挟まれている部分を読み込む
lexBracketed open close
 = do { lexUntilSep open                     -- 字句開始位置手前まで読み飛ばす
      ; lexSkipChar                          -- 字句開始文字読み捨てる
      ; b <- lexUntilSep close               -- 字句終了位置手前まで読み込む
      ; lexSkipChar                          -- 字句終了文字読み捨てる
      ; return b                             -- 字句を返す
      }

lex_h, lex_l, lex_u, lex_t, lex_r, lex_s, lex_b :: Lexer Lexeme
lex_h = lexWord                -- リモートホスト         
lex_l = lexWord                -- リモートログ名         
lex_u = lexWord                -- リモートユーザー       
lex_t = lexBracketed '[' ']'   -- リクエスト受付け時刻
lex_r = lexBracketed '"' '"'   -- リクエストライン       
lex_s = lexWord                -- ステータス     
lex_b = lexWord                -- レスポンスボディ長


-- Lexer 部品で構成した accessLog

accessLog :: String -> [String]
accessLog = lexResult $ sequence [lex_h,lex_l,lex_u,lex_t,lex_r,lex_s,lex_b]
