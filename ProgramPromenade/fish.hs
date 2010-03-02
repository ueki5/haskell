fish :: Painter
fish = segmentsToPainter 80 80
    [[(40,60),(33,63),(27,65),(20,64),(17,67),(12,72),(5,77),(0,80),(-2,72),
      (-4,60),(-4,50),(-4,44),(-12,38),(-16,30),(-20,20),(0,0),(20,20),(30,16),
      (38,12),(44,4),(50,4),(60,4),(72,2),(80,0),(75,3),(68,8),(63,13),(60,16),
      (53,15),(46,17),(40,20),(32,32),(40,40)],[(0,64),(0,54),(4,58),(0,64)],
     [(8,68),(8,58),(12,60),(8,68)],[(8,54),(16,42),(28,26),(40,16),(58,10)],
     [(-4,44),(6,28),(20,20)],[(-2,36),(-8,30),(-12,22)],
     [(2,30),(-6,22),(-8,16)],[(8,24),(-2,16),(-4,10)],[(10,18),(2,10),(0,6)],
     [(20,64),(24,56),(26,44),(32,32)],[(26,56),(30,58),(34,58),(40,54)],
     [(28,50),(32,52),(36,52),(40,50)],[(30,42),(34,46),(40,46)],
     [(38,36),(40,34)],[(38,32),(40,30)],[(36,30),(40,26)]]

rot45 :: Painter -> Painter                 -- 左上を中心に45度の回転
rot45 p = transformPainter p (0.5,0.5) (1,1) (0,1)
over :: Painter -> Painter -> Painter       -- 2枚のぺインタを重ねる
over p q = \ frame -> do transformPainter p (0,0) (1,0) (0,1) frame
                         transformPainter q (0,0) (1,0) (0,1) frame

fish2,u,t :: Painter
fish2 = flipHoriz (rot45 fish)
u = over (over fish2 (rot fish2))           -- 図-5のL
         (over (rot(rot fish2)) (rot(rot(rot fish2))))
t = over fish (over fish2 (rot(rot(rot fish2))))    -- 図-5のK

nonet :: Painter -> Painter -> Painter -> Painter -> Painter -> 
         Painter -> Painter -> Painter -> Painter -> Painter        --p q r
nonet p q r s t u v w x =                                           --s t u
  above 1 2 (beside 1 2 p (q <-> r))                                --v w x
            ((beside 1 2 s (t <-> u)) </> (beside 1 2 v (w <-> x)))

square, corner, side :: Int -> Painter
square n =
  nonet (corner n) (side n) (rot(rot(rot(corner n))))
        (rot(side n)) u (rot(rot(rot(side n))))
        (rot(corner n)) (rot(rot(side n))) (rot(rot(corner n)))

quartet :: Painter -> Painter -> Painter -> Painter -> Painter
quartet p q r s = (p <-> q) </> (r <-> s)

corner 0 = blank
corner (n+1) = quartet (corner n) (side n) (rot(side n)) u

side 0 = blank
side (n+1) = quartet (side n) (side n) (rot t) t

main = square 2 unitSquare
