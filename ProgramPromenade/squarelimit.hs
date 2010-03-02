import Numeric
type Vect = (Float, Float)          -- �٥�����
type LineSegment = [Vect]           -- ��ʬ
type Figure = [LineSegment]         -- �޷��ǡ�������ʬ�Υꥹ��
type Frame = (Vect,Vect,Vect)       -- �Ǹ�γ��������ե졼��
type Painter = (Frame -> IO ())     -- �ڥ��󥿤ϥե졼����㤤���Ϥ���
blan,wave :: Painter
blank =  \ frame -> putStr ""       -- ���������ʤ��ڥ���

wave = segmentsToPainter 20 20      -- ��-3 G�οͤ���
    [[(0,13),(3,8),(6,12),(7,11),(5,0)],[(8,0),(10,6),(12,0)],
     [(15,0),(12,10),(20,3)],[(20,7),(15,13),(12,13),(13,17),(12,20)],
     [(8,20),(7,17),(8,13),(6,13),(3,12),(0,17)]]

infixr 7 +~, -~                     -- 2��黻�� +~,-~
infixr 8 *~                         -- 2��黻�� *~
(+~), (-~) :: Vect -> Vect -> Vect
(x0,y0) +~ (x1,y1) = (x0+x1, y0+y1) -- �٥���(x0,y0)��(x1,y1)��­��
(x0,y0) -~ (x1,y1) = (x0-x1, y0-y1) -- �٥���(x0,y0)����(x1,y1)�����
(*~) :: Float -> Vect ->Vect
a *~ (x,y) = (a*x, a*y)             -- �٥���(x0,y0)��a�ܤ���

drawLine,drawLine' :: [Vect] -> String      --[(x,y),...]�����������ʸ�������
drawLine ((x,y):xys) = show x ++ " " ++ show y ++ " moveto\n" ++ drawLine' xys
drawLine' [] = ""
drawLine' ((x,y):xys) = show x ++ " " ++ show y ++ " lineto\n" ++ drawLine' xys

segmentsToPainter :: Float -> Float -> Figure -> Painter
segmentsToPainter scale0 scale1 segs =      -- ��ʬ�ΰ��֤����������ڥ��󥿤�
 \ frame -> putStr \$
            let toFrame (x,y) = frameCoodMap frame (x/scale0, y/scale1)
                drawSeg seg = drawLine (map toFrame seg)
             in
              concat (map drawSeg segs) ++ "stroke\n"

frameCoodMap :: Frame -> (Vect -> Vect)
frameCoodMap (org,edge0,edge1) =            -- �ե졼�������غ�ɸ�Ѵ�
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
rot p = transformPainter p (1,0) (1,1) (0,0)        -- ����ž
flipHoriz p = transformPainter p (1,0) (0,0) (1,1)  -- ����ȿž
flipVert p = transformPainter p (0,1) (1,1) (0,0)   -- �岼ȿž

above,beside :: Float -> Float -> Painter -> Painter -> Painter
above m n p q =                     -- p��q�ξ��m:n�ι⤵���Ѥ�
 \ frame -> do transformPainter p (0,r) (1,r) (0,1) frame
               transformPainter q (0,0) (1,0) (0,r) frame
      where r = n/(m+n)
beside m n p q =                    -- p��q��m:n�������¤٤�
 \ frame -> do transformPainter p (0,0) (r,0) (0,1) frame
               transformPainter q (r,0) (1,0) (r,1) frame
      where r = m/(m+n)

infixr 3 </>                        -- 2��黻�� </>
infixr 4 <->                        -- 2��黻�� <->
(</>), (<->) :: Painter -> Painter -> Painter
(</>) = above 1 1                   -- p </> q  p��q�ξ���Ѥ�
(<->) = beside 1 1                  -- p <-> q  p��q���¤٤�

unitSquare :: Frame
unitSquare = ((128,16),(256,0),(0,256))     -- ���������ե졼��
