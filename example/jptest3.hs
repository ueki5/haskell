import qualified System.IO.UTF8 as U
data Person = Person {name :: String,
                      age  :: Int}
instance Show Person where
    show (Person name age) = name ++ " " ++ show age
main = U.print $ Person "太郎" 20
