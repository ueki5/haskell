-- crowd of functional programmers

import Numeric
type Vect = (Float, Float)
type Figure = [LineSegment]
type LineSegment = [Vect]
type Frame = (Vect,Vect,Vect)
type Painter = (Frame -> IO ())
blank =  \ frame -> putStr ""

infixr 7 +~, -~ 
infixr 8 *~
(+~), (-~) :: Vect -> Vect -> Vect
(x0,y0) +~ (x1,y1) = (x0+x1, y0+y1)        -- addVect
(x0,y0) -~ (x1,y1) = (x0-x1, y0-y1)        -- subVect
(*~) :: Float -> Vect ->Vect
a *~ (x,y) = (a*x, a*y)                    -- scaleVect

man :: Painter
man = segmentsToPainter 20 20
    [[(6,10),(0,10),(0,12),(6,12),(6,14),(4,16),(4,18),(6,20),(8,20),(10,18),
      (10,16),(8,14),(8,12),(10,12),(10,14),(12,14),(12,10),(8,10),(8,8),
      (10,0),(8,0),(7,4),(6,0),(4,0),(6,8),(6,10)]]

unitSquare :: Frame
unitSquare = ((128,16),(256,0),(0,256))

drawLine,drawLine' :: [Vect] -> String
drawLine ((x,y):xys) = show x ++ " " ++ show y ++ " moveto\n" ++ drawLine' xys
drawLine' [] = ""
drawLine' ((x,y):xys) = show x ++ " " ++ show y ++ " lineto\n" ++ drawLine' xys

segmentsToPainter :: Float -> Float -> Figure -> Painter
segmentsToPainter scale0 scale1 segs =
 \ frame -> putStr $
            let toFrame (x,y) = frameCoodMap frame (x/scale0, y/scale1)
                drawSeg seg = drawLine (map toFrame seg)
             in
              concat (map drawSeg segs) ++ "stroke\n"

frameCoodMap :: Frame -> (Vect -> Vect)
frameCoodMap (org,edge0,edge1) = 
 \ (x,y) -> org +~ x *~ edge0 +~ y *~ edge1

transformPainter :: Painter -> Vect -> Vect -> Vect -> Painter
transformPainter painter org edge0 edge1 =
 \ frame ->
    let m = frameCoodMap frame
        newOrg = m org
        newEdge0 = m edge0 -~ newOrg
        newEdge1 = m edge1 -~ newOrg
      in painter (newOrg, newEdge0, newEdge1)
above,beside :: Float -> Float -> Painter -> Painter -> Painter
above m n p q = 
 \ frame -> do transformPainter p (0,r) (1,r) (0,1) frame
               transformPainter q (0,0) (1,0) (0,r) frame
      where r = n/(m+n)
beside m n p q =
 \ frame -> do transformPainter p (0,0) (r,0) (0,1) frame
               transformPainter q (r,0) (1,0) (r,1) frame
      where r = m/(m+n)
horiz :: Float -> Painter -> Painter
horiz 0 _ = blank
horiz n p = beside 1 (n-1) p (horiz (n-1) p)

crowd :: Painter -> Painter
crowd p = above (1-s10) s10 blank
           (above (inv 10) s9 (horiz 10 p)
             (above (inv 9) s8 (horiz 9 p)
               (above (inv 8) s7 (horiz 8 p)
                 (above (inv 7) s6 (horiz 7 p)
                   (above (inv 6) (inv 5) (horiz 6 p) (horiz 5 p))))))
            where inv n = fromRational (1/n)
                  s10 = sum (map inv [5..10])
                  s9 = sum (map inv [5..9])
                  s8 = sum (map inv [5..8])
                  s7 = sum (map inv [5..7])
                  s6 = sum (map inv [5..6])

main = crowd man unitSquare
