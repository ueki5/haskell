fiblst::Num a => a -> [a]
fiblst 0 = [fib 0]
fiblst n =  fiblst (n - 1) ++ [fib n]
fib::Num a => a -> a
fib = fibIter 0 1
    where
      fibIter a b 0 = a
      fibIter a b n = fibIter b (a+b) (n-1)
ueki::IO ()
ueki = do putStr "UEKIGO"
          putStrLn "!!!!!!"