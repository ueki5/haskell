import Numeric
type Vect = (Float, Float)          -- ベクタ型
type LineSegment = [Vect]           -- 線分
type Figure = [LineSegment]         -- 図形データは線分のリスト
type Frame = (Vect,Vect,Vect)       -- 最後の絵を描くフレーム
type Painter = (Frame -> IO ())     -- ぺインタはフレームを貰い出力する
blan,wave :: Painter
blank =  \ frame -> putStr ""       -- 何も描かないぺインタ

wave = segmentsToPainter 20 20      -- 図-3 Gの人がた
    [[(0,13),(3,8),(6,12),(7,11),(5,0)],[(8,0),(10,6),(12,0)],
     [(15,0),(12,10),(20,3)],[(20,7),(15,13),(12,13),(13,17),(12,20)],
     [(8,20),(7,17),(8,13),(6,13),(3,12),(0,17)]]

infixr 7 +~, -~                     -- 2項演算子 +~,-~
infixr 8 *~                         -- 2項演算子 *~
(+~), (-~) :: Vect -> Vect -> Vect
(x0,y0) +~ (x1,y1) = (x0+x1, y0+y1) -- ベクタ(x0,y0)に(x1,y1)を足す
(x0,y0) -~ (x1,y1) = (x0-x1, y0-y1) -- ベクタ(x0,y0)から(x1,y1)を引く
(*~) :: Float -> Vect ->Vect
a *~ (x,y) = (a*x, a*y)             -- ベクタ(x0,y0)をa倍する

drawLine,drawLine' :: [Vect] -> String      --[(x,y),...]を結ぶ線を引く文字列を作る
drawLine ((x,y):xys) = show x ++ " " ++ show y ++ " moveto\n" ++ drawLine' xys
drawLine' [] = ""
drawLine' ((x,y):xys) = show x ++ " " ++ show y ++ " lineto\n" ++ drawLine' xys

segmentsToPainter :: Float -> Float -> Figure -> Painter
segmentsToPainter scale0 scale1 segs =      -- 線分の位置を正規化しぺインタに
 \ frame -> putStr \$
            let toFrame (x,y) = frameCoodMap frame (x/scale0, y/scale1)
                drawSeg seg = drawLine (map toFrame seg)
             in
              concat (map drawSeg segs) ++ "stroke\n"

frameCoodMap :: Frame -> (Vect -> Vect)
frameCoodMap (org,edge0,edge1) =            -- フレームの枠内へ座標変換
 \ (x,y) -> org +~ x *~ edge0 +~ y *~ edge1

transformPainter :: Painter -> Vect -> Vect -> Vect -> Painter
transformPainter painter org edge0 edge1 =
 \ frame ->
    let m = frameCoodMap frame
        newOrg = m org
        newEdge0 = m edge0 -~ newOrg
        newEdge1 = m edge1 -~ newOrg
      in painter (newOrg, newEdge0, newEdge1)

rot,flipHoriz,flipVert :: Painter -> Painter
rot p = transformPainter p (1,0) (1,1) (0,0)        -- 左回転
flipHoriz p = transformPainter p (1,0) (0,0) (1,1)  -- 左右反転
flipVert p = transformPainter p (0,1) (1,1) (0,0)   -- 上下反転

above,beside :: Float -> Float -> Painter -> Painter -> Painter
above m n p q =                     -- pをqの上にm:nの高さで積む
 \ frame -> do transformPainter p (0,r) (1,r) (0,1) frame
               transformPainter q (0,0) (1,0) (0,r) frame
      where r = n/(m+n)
beside m n p q =                    -- pとqをm:nの幅で並べる
 \ frame -> do transformPainter p (0,0) (r,0) (0,1) frame
               transformPainter q (r,0) (1,0) (r,1) frame
      where r = m/(m+n)

infixr 3 </>                        -- 2項演算子 </>
infixr 4 <->                        -- 2項演算子 <->
(</>), (<->) :: Painter -> Painter -> Painter
(</>) = above 1 1                   -- p </> q  pをqの上に積む
(<->) = beside 1 1                  -- p <-> q  pとqを並べる

unitSquare :: Frame
unitSquare = ((128,16),(256,0),(0,256))     -- 絵を描くフレーム
