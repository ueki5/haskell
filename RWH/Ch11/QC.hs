import Debug.Trace
prop_eq xs = traceShow xs $ 
   xs == reverse (reverse xs)
