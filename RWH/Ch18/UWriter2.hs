calc = let a = 1
           b = a + 2
           c = b + 3
       in c
type Log = (Int ,String)
(+++) :: String -> String -> String
(+++) a b = if a == "" then b else a ++ "," ++ b
add :: Log -> Int -> Log
add (a, s) b = (a + b, s +++ (show b))
calc2 = let a = (0, "") `add` 1
            b = a `add` 2
            c = b `add` 3
        in c
