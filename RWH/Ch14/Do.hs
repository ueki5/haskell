module Ch14.Do where
doNotation3 = do
  x <- xs
  return x
  where xs = [0,1,2]
-- translated3 = 
--   let f x = do xs
--                return x
--   where xs = [0,1,2]
--         ys = [9,10]
robust :: [a] -> Maybe a
robust xs = do 
  (_:x:_) <- Just xs
  return x
robust' xs = 
  Just xs >>= \(_:x:_) -> 
  return x
robust'' xs = 
  let f (_:x:_) = return x
      f _ = fail ""
  in Just xs >>= f
robust''' xs = 
  Just xs >>= f
  where f (_:x:_) = return x
        f _ = fail ""
wordCount = print . length . words =<< getContents