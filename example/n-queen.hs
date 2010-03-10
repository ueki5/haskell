import Monad
import System
import IO
import Control.Monad.Error

-- set1 内の文字と対応する set2 内の文字に変換する
translate :: String -> String -> Char -> Char
translate []     _      c = c
translate (x:xs) []     c = if x == c then ' ' else translate xs []  c
translate (x:xs) [y]    c = if x == c then  y  else translate xs [y] c
translate (x:xs) (y:ys) c = if x == c then  y  else translate xs ys  c

-- 文字列全体を変換する
translateString :: String -> String -> String -> String
translateString set1 set2 str = map (translate set1 set2) str

usage :: IOError -> IO ()
usage e = do putStrLn "Usage: ex14 set1 set2"
             putStrLn "Translates characters in set1 on stdin to the corresponding"
             putStrLn "characters from set2 and writes the translation to stdout."

-- コマンドライン引数にしたがって、標準入力を標準出力へ変換する
main :: IO ()
main = (do [set1,set2] <- getArgs
           contents    <- hGetContents stdin
           putStr $ translateString set1 set2 contents)
       `catchError` usage

-- map関数
map                     :: (a->b) -> [a] -> [b]
map f  []               =  []
map f (x:xs)            =  f x : map f xs
