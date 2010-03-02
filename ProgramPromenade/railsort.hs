
decMergen, incMergen    :: [[Int]] -> [Int]     -- �����Υꥹ�ȤΥꥹ�Ȥ��餤�ꥹ�Ȥ��֤�
decMergen (xs0:xs1:[])  = decMerge2 xs0 xs1     -- �ꥹ�Ȥ�2�ĤΤȤ� �ޡ���
decMergen (xs0:xs1:xss) = decMergen ((decMerge2 xs0 xs1):xss)   -- 3�İʾ�ΤȤ�
incMergen (xs0:xs1:[])  = incMerge2 xs0 xs1
incMergen (xs0:xs1:xss) = incMergen ((incMerge2 xs0 xs1):xss)

decMerge2, incMerge2    :: [Int]->[Int]->[Int]  -- 2�ĤΥꥹ�ȤΥޡ��� Curry��
decMerge2 [] ys         = ys                    -- ���Υꥹ�Ȥ����ä�
decMerge2 (x:xs) []     = x:xs                  -- ��Υꥹ�Ȥ����ä�
decMerge2 (x:xs)(y:ys)
 | x>y                  = x:(decMerge2 xs (y:ys))     -- �������ۤ�����Ϥ�
 | otherwise            = y:(decMerge2 (x:xs) ys)
incMerge2 [] ys         = ys
incMerge2 (x:xs) []     = x:xs
incMerge2 (x:xs)(y:ys)
 | x<y                  = x:(incMerge2 xs (y:ys))
 | otherwise            = y:(incMerge2 (x:xs) ys)

decSort, incSort :: Int -> [Int] -> [Int]       -- �ǡ���Ĺ �����Ȥ���ꥹ�� ��̥ꥹ��
decSort n cs
 | n == 2    = decMergen ([head cs]:[tail cs])  -- 2�ĤΤȤ� �ꥹ�Ȥˤ��ƥޡ���
 | otherwise = decMergen (xs:ys)                -- 4�İʾ�ΤȤ� �����Ⱦʬ��
                 where xs = reverse (incSort n2 (take n2 cs))   -- ��Ⱦ�򥽡���
                       ys = decMove n2 (drop n2 cs)             -- ��Ⱦ��ʬ��
                       n2 = n `div` 2
incSort n cs
 | n == 2    = incMergen ([head cs]:[tail cs])
 | otherwise = incMergen (xs:ys)
                 where xs = reverse (decSort n2 (take n2 cs))
                       ys = incMove n2 (drop n2 cs)
                       n2 = n `div` 2

decMove, incMove  :: Int -> [Int] -> [[Int]]    -- �����å���ʬ�ۤ���
decMove n cs
 | n == 2    = [head cs]:[tail cs]
 | otherwise = xs:ys
                 where xs = decSort n2 (take n2 cs)     -- �����Ⱦʬ�� ��Ⱦ������
                       ys = decMove n2 (drop n2 cs)     -- ��Ⱦ ʬ��
                       n2 = n `div` 2
incMove n cs
 | n == 2    = [head cs]:[tail cs]
 | otherwise = xs:ys
                 where xs = incSort n2 (take n2 cs)
                       ys = incMove n2 (drop n2 cs)
                       n2 = n `div` 2

cars8 :: [Int]              -- �ƥ����ѥǡ���
cars8 = [3,1,4,5,2,6,7,0]
