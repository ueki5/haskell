module Easter (easter) where

-- 復活祭

import Time                         -- Module TimeのMonth型を使うため

easter :: Int -> (Month, Int)
easter y = 
 if n''>31 then (April,n''-31) else (March,n'')     -- E8
  where
    n''= n' + 7 - (d + n') `mod` 7                  -- E7
      where n'= if n < 21 then n + 30 else n        -- E6
            n = 44 - e'
    e'= if e == 25 && g > 11 || e == 24 then e + 1 else e   -- E5
      where e = (11 * g + 20 + z - x) `mod` 30
    d = (5 * y) `div` 4 - x - 10                    -- E4
    x = 3 * c `div` 4 - 12                          -- E3
    z = (8 * c + 5) `div` 25 - 5    -- `div`,`mod`は*,/と同じ優先度
    c = y `div` 100 + 1                             -- E2
    g = y `mod` 19 + 1                              -- E1
