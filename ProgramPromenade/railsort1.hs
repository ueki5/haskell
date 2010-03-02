{-
���(������)�Υץ����

��⡤4���� Haskell ��Ϣ�ܤ�������ɤޤ��Ƥ��������ޤ�����
������μ�ξ�Υ���������Ǥ��������٤Ƥδؿ��ˤĤ��� dec�Ϥ�inc�Ϥ�
��ĤΥС��������Ѱ� (Ʊ���褦�ʥ����ɤ���ʣ���Ƥ���) �Ȥ���
���ˤʤꡤ���Τ褦�ʥץ������äƤߤޤ�����

����Ū�ˤϡ��ִط��黻�פ�����˻������뤳�Ȥˤ���ΤǤ�����
dec ����� inc ��Ƥӡ�inc ����� dec ��Ƥ���ꤹ�뤳�Ȥ�����Τǡ�
�ط��黻 (������ؿ�) ��ڥ��ˤ��ƻ����⤯���Ȥˤ��ޤ�����
�ؿ��� First Class �Ǥ����顤�����������Ȥ���ñ�ˤǤ��ޤ��͡�
(�ڥ��ˤ������դδط�����������С�not ��������ƴؿ����롤�Ȥ���
��ˡ�⤢��ޤ������Ƶ��Υͥ��Ȥ������ʤ�ȡ�not . not . not . ....
�ȡ�not �����Ťˤ�ʤäƤ��ޤ��Ȥ�����������ޤ���Ǥ�����)

�����ɤϤ����֤�û���ʤä��Ȼפ��ޤ���
-}

merge2 :: (Int -> Int -> Bool) -> [Int] -> [Int] -> [Int]
merge2 rel [] ys = ys
merge2 rel (x:xs) [] = x:xs
merge2 rel (x:xs) (y:ys) | x `rel` y = x : merge2 rel xs (y:ys)
                         | otherwise = y : merge2 rel (x:xs) ys

mergen :: (Int -> Int -> Bool) -> [[Int]] -> [Int]
mergen rel = foldl1 (merge2 rel)

decMergen = foldl1 (merge2 (>)) --- ư���ǧ�ѡ������Ϥ���ʤ�
incMergen = foldl1 (merge2 (<)) --- ư���ǧ�ѡ������Ϥ���ʤ�

sort :: (Int -> Int -> Bool, Int -> Int -> Bool) -> Int -> [Int] -> [Int]
sort (rel1,rel2) n cs
  | n == 2 =    mergen rel1 ([head cs]:[tail cs])
  | otherwise = mergen rel1 (xs:ys)
                where xs = reverse (sort (rel2,rel1) n2 (take n2 cs))
                      ys = move (rel1,rel2) n2 (drop n2 cs)
                      n2 = n `div` 2

decSort, incSort :: Int -> [Int] -> [Int]
decSort = sort ((>),(<))
incSort = sort ((<),(>))

move :: (Int -> Int -> Bool, Int -> Int -> Bool) -> Int -> [Int] -> [[Int]]
move rels n cs | n == 2    = [head cs]:[tail cs]
               | otherwise = xs:ys
                             where xs = sort rels n2 (take n2 cs)
                                   ys = move rels n2 (drop n2 cs)
                                   n2 = n `div` 2

decMove, incMove :: Int -> [Int] -> [[Int]]
decMove = move ((>),(<)) --- ư���ǧ�ѡ������Ϥ���ʤ�
incMove = move ((<),(>)) --- ư���ǧ�ѡ������Ϥ���ʤ�

cars8 :: [Int]
cars8 = [3,1,4,5,2,6,7,0]
