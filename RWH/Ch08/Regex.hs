module Ch08.MyRegex where
import Text.Regex.Posix
mymatch :: String -> String -> [String] -> [String]
mymatch source regex results = 
    let (skip, hit, rest) = (source =~ regex :: (String, String, String)) 
        results' = if hit == [] 
                   then results
                   else hit:results
    in case rest of
         [] -> results'
         _  -> mymatch rest regex results'
(=^) :: String -> String -> [String]
(=^) src regex = mymatch src regex []
