type Order     = Int
type Direction = (Int,Int)

space   = showString " "
newline = showString "\n"

left, right :: Direction -> Direction
left  (dx,dy) = (-dy,dx)
right (dx,dy) = (dy,-dx)

f :: Direction -> ShowS
f (dx,dy) = shows dx . space . shows dy . space . showString "rlineto" . newline

{-
-- IO モナドではなく ShowS を使い，バインド(>>)ではなく，関数合成(.)で繋ぐ

x 0     _       = id
x (n+1) (x0,y0) = y n (x1,y1) . f (x1,y1) . x n (x0,y0) . f (x0,y0)
                . x n (x0,y0) . f (x3,y3) . y n (x3,y3)
  where (x1,y1) = left (x0,y0); (x3,y3) = right (x0,y0)

-- これを List 上のたたみこみに変形して

x (n+1) (x0,y0)
 = foldr (.) id [ x n (x3,y3) , f (x3,y3) , y n (x0,y0) , f (x0,y0)
                , y n (x0,y0) , f (x1,y1) , x n (x1,y1) ]
  where (x1,y1) = left (x0,y0); (x3,y3) = right (x0,y0)

-- さらに where 節やめて本体に展開すると

x (n+1) (x0,y0)
 = foldr (.) id [ x n (right (x0,y0)), f (right (x0,y0)), y n (x0,y0), f (x0,y0)
                , y n (x0,y0) , f (left (x0,y0)) , x n (left (x0,y0)) ]

-- こうしておいて，(x0,y0) を map で括りだすと

x (n+1) (x0,y0)
 = foldr (.) id (map ($ (x0,y0)) [ x n . right, f . right
                                 , y n, f, y n
                                 , f . left, x n . left ])

y 0     _       = id
y (n+1) (x0,y0) = x n (x3,y3) . f (x3,y3) . y n (x0,y0) . f (x0,y0)
                . y n (x0,y0) . f (x1,y1) . x n (x1,y1)
  where (x1,y1) = left (x0,y0); (x3,y3) = right (x0,y0)
-}

interp v = foldr (.) id . map ($ v)

x,y :: Order -> Direction -> ShowS

x 0     _ = id
x (n+1) d = interp d [ y n . left
                     , f . left
                     , x n
                     , f
                     , x n
                     , f . right
                     , y n . right
                     ]

y 0     _ = id
y (n+1) d = interp d [ x n . right
                     , f . right
                     , y n
                     , f
                     , y n
                     , f . left
                     , x n . left
                     ]

hilbert :: Int -> Int -> ShowS
hilbert size n 
 = shows o . space . shows o . space . showString "moveto" . newline
 . x n (x0,y0) . showString "stroke"
  where x0 = size `div` (2 ^ n)
        y0 = 0
        o  = x0 `div` 2

main = putStrLn $ hilbert 256 5 ""
