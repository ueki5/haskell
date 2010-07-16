module Ch07.Actions2 where
str2action :: String -> IO ()
str2action input = putStrLn ("Data: " ++ input)

list2actions :: [String] -> [IO ()]
list2actions = map str2action

numbers :: [Int]
numbers = [1..10]

strings :: [String]
strings = map show numbers

actions :: [IO ()]
actions = list2actions strings

printitall :: IO ()
printitall = runall actions

runall :: [IO ()] -> IO ()
runall [] = return ()
runall (x:xs) = 
    do x
       runall xs

main = do str2action "Start of the program"
          mapM_ str2action strings
          str2action "Done!"
