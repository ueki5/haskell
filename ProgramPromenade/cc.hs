-- ξ������(Count Change Problem)

type Amount = Integer
type Coin   = Integer
type Count  = Integer

-- ��ۤȲ�ʾ(����)�Υꥹ�Ȥ��顤ξ�ؤξ��ο���

cc :: Amount -> [Coin] -> Count
cc 0 _  = 1                 -- ��ۤ����礦��0�ʤ顤ξ�ؤ�1�̤�
cc _ [] = 0                 -- ξ�ؤ˻Ȥ���ʾ���ʤ���С�ξ�ؤ�0�̤�
cc a ccs@(c:cs)
 | a < 0     = 0            -- ��ۤ�0��꾯�ʤ���С�ξ�ؤ�0�̤�
 | otherwise = cc (a-c) ccs -- �ǽ�μ���β�ʾ���̤��������ۤ�ξ�ؤξ��ο�
             + cc a     cs  -- �ǽ�μ���β�ʾ�ʳ���Ȥ�ξ�ؤξ��ο�

