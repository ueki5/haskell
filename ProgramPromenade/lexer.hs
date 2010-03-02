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

lexSkipChar :: Lexer ()                         -- ����ʸ�������Ƭ��1ʸ�����ɤ߼ΤƤ�
lexSkipChar = modify $ \ s -> if null s then s else tail s

-- modify �� Control.Monad.State ���������Ƥ��롥
-- modify :: (MonadState s m) => (s -> s) -> m ()
-- modify f = do { s <- get; return (f s) }

lexUntilSep :: Char -> Lexer Lexeme          -- ���ꤷ�����ڤ�ʸ����ľ���ޤǤ��ɤ߹���
lexUntilSep sep = State $ break (sep==)

lexBracketed :: Char -> Char -> Lexer Lexeme -- ���ꤷ��ʸ���˶��ޤ�Ƥ�����ʬ���ɤ߹���
lexBracketed open close
 = do { lexUntilSep open                     -- ���糫�ϰ��ּ����ޤ��ɤ����Ф�
      ; lexSkipChar                          -- ���糫��ʸ���ɤ߼ΤƤ�
      ; b <- lexUntilSep close               -- ���罪λ���ּ����ޤ��ɤ߹���
      ; lexSkipChar                          -- ���罪λʸ���ɤ߼ΤƤ�
      ; return b                             -- ������֤�
      }

lex_h, lex_l, lex_u, lex_t, lex_r, lex_s, lex_b :: Lexer Lexeme
lex_h = lexWord                -- ��⡼�ȥۥ���         
lex_l = lexWord                -- ��⡼�ȥ�̾         
lex_u = lexWord                -- ��⡼�ȥ桼����       
lex_t = lexBracketed '[' ']'   -- �ꥯ�����ȼ��դ�����
lex_r = lexBracketed '"' '"'   -- �ꥯ�����ȥ饤��       
lex_s = lexWord                -- ���ơ�����     
lex_b = lexWord                -- �쥹�ݥ󥹥ܥǥ�Ĺ


-- Lexer ���ʤǹ������� accessLog

accessLog :: String -> [String]
accessLog = lexResult $ sequence [lex_h,lex_l,lex_u,lex_t,lex_r,lex_s,lex_b]
