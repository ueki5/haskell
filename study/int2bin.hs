int2bin :: Int -> Int -> [Int]
int2bin n m = int2bin' n []
            where 
              int2bin' :: Int -> [Int] -> [Int]
              int2bin' 0 ns = ns
              int2bin' n ns = int2bin' (div n m) ((mod n m):ns)
                