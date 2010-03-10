import System.IO.UTF8
strlen::Num a => String -> a
strlen [] = 0
strlen (x:xs) = 1 + strlen xs
