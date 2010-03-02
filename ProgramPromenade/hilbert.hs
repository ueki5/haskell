import Numeric

left,right    :: (Int,Int) -> (Int,Int)
left  (dx,dy) = (-dy, dx)           -- ����
right (dx,dy) = (dy, -dx)           -- ����

f   :: (Int,Int) -> IO ()           -- F���б�����, rlineto�����а��֤ޤ��������
f   (dx,dy) = putStrLn ((show dx) ++ " " ++ (show dy) ++  " rlineto")

x,y :: Int -> (Int,Int) -> IO ()    -- X,Y���б�
x 0 (_,_) = putStr ""               -- 0���ʤ�ʤˤ⤷�ʤ�. �������
x (n+1) (x0,y0) = do y n (x1,y1); f (x1,y1); x n (x0,y0); f (x0,y0)
                     x n (x0,y0); f (x3,y3); y n (x3,y3)
  where (x1,y1) = left (x0,y0); (x3,y3) = right (x0,y0)

y 0 (_,_) = putStr ""
y (n+1) (x0,y0) =do x n (x3,y3); f (x3,y3); y n (x0,y0); f (x0,y0)
                    y n (x0,y0); f (x1,y1); x n (x1,y1)
  where (x1,y1) = left (x0,y0); (x3,y3) = right (x0,y0)

hilbert :: Int -> Int -> IO ()      -- �ɥ饤��
hilbert size n = do putStrLn ((show o) ++ " " ++ (show o) ++  " moveto")
                    x n (x0,y0) 
                    putStrLn "stroke"
      where x0 = size `div` (2 ^ n) -- ��������Ĺ (= ��ư��Υ)
            y0 = 0
            o  = x0 `div` 2         -- ���������ȯ���֤ޤǤε�Υ

main = hilbert 256 5
