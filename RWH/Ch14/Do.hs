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
