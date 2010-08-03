module Ch08.GlobRegex
    (
     globToRegex
     ,matchesGlob
    ) where

import Text.Regex.Posix ((=~))
globToRegex :: String -> String
globToRegex cs = '^' : globToRegex' cs ++ "$"
globToRegex' :: String -> String
globToRegex' "" = ""
globToRegex' ('*':cs) = ".*" ++ globToRegex' cs
globToRegex' ('?':cs) = "." ++ globToRegex' cs
globToRegex' ('[':'!':c:cs) = "[^" ++ c:charClass cs
globToRegex' ('[':c:cs)     = "["  ++ c:charClass cs
globToRegex' ('[':_) = error "unterminated character class"
globToRegex' (c:cs) = escape c ++ globToRegex' cs
escape c | c `elem` regexChars = '\\' : [c]
         | otherwise = [c]
    where regexChars = "\\+()^$.{}]|"
charClass (']':cs) = ']' : globToRegex' cs
charClass (c :cs) = c : charClass cs
charClass [] = error "unterminated character class"
matchesGlob :: FilePath -> String -> Bool
matchesGlob name pat = name =~ globToRegex' pat 

main = do
  ret <- return ("foo.c" =~ globToRegex "f??.c" :: String)
  print ret
  ret <- return ("test.c" =~ globToRegex "t[ea]s*" :: String)
  print ret
  ret <- return ("taste.txt" =~ globToRegex "t[ea]s*" :: String)
  print ret
