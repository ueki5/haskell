module Ch14.CartesianProduct where
conprehensive xs ys = [(x, y)| x <- xs, y <- ys]
monadic xs ys = do {x <- xs; y <- ys; return (x, y)}
blockDo xs ys = do
  x <- xs
  y <- ys
  return (x, y)
blockPlain xs ys =
    xs >>=
    \x -> ys >>=
    \y -> return (x, y)
blockPlain_reloaded xs ys =
    concat (map (\x -> 
                 concat (map (\y -> 
                              return (x, y)) 
                         ys)) 
            xs)
--xs >>= f = concat (map f xs)