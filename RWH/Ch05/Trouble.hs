import Data.Char
upcaseFirst (x:xs) = (toUpper x):xs
camelCase :: String -> String
camelCase xs = concat (map upcaseFirst (words xs))