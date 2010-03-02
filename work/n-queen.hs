import Monad
import System
import IO
import Control.Monad.Error

-- set1 ���̕����ƑΉ����� set2 ���̕����ɕϊ�����
translate :: String -> String -> Char -> Char
translate []     _      c = c
translate (x:xs) []     c = if x == c then ' ' else translate xs []  c
translate (x:xs) [y]    c = if x == c then  y  else translate xs [y] c
translate (x:xs) (y:ys) c = if x == c then  y  else translate xs ys  c

-- ������S�̂�ϊ�����
translateString :: String -> String -> String -> String
translateString set1 set2 str = map (translate set1 set2) str

usage :: IOError -> IO ()
usage e = do putStrLn "Usage: ex14 set1 set2"
             putStrLn "Translates characters in set1 on stdin to the corresponding"
             putStrLn "characters from set2 and writes the translation to stdout."

-- �R�}���h���C�������ɂ��������āA�W�����͂�W���o�͂֕ϊ�����
main :: IO ()
main = (do [set1,set2] <- getArgs
           contents    <- hGetContents stdin
           putStr $ translateString set1 set2 contents)
       `catchError` usage

-- map�֐�
map                     :: (a->b) -> [a] -> [b]
map f  []               =  []
map f (x:xs)            =  f x : map f xs
