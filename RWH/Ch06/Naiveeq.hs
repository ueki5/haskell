module Ch06.Naiveeq where 
data Color = Red | Green | Blue
           deriving (Eq, Ord)

colorEq :: Color -> Color -> Bool
colorEq Red Red = True
colorEq Green Green = True
colorEq Blue Blue = True
colorEq _ _ = False

stringEq :: String -> String -> Bool
stringEq [] [] = True
stringEq (x:xs) (y:ys) = x == y && stringEq xs ys
stringEq _ _ = False

instance Show Color where
    show Red = "Red"
    show Green = "Green"
    show Blue = "Blue"

instance Read Color where
    readsPrec _ value = 
          tryParse [("Red", Red), ("Green", Green), ("Blue", Blue)] 
        where trimvalue = trim value
              tryParse [] = []
              tryParse ((attempt, result):xs) =
                  if (take (length attempt) trimvalue) == attempt
                   then [(result, drop (length attempt) trimvalue)]
                    else tryParse xs
trim :: String -> String
trim [] = []
trim all@(x:xs) = if x == ' '
                  then trim xs
                  else all