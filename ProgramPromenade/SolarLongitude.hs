module SolarLongitude (lon) where

-- ���۲���

sind d = sin (d * pi / 180)         -- ��ñ�̤λ��Ѵؿ�
cosd d = cos (d * pi / 180)
tand d = tan (d * pi / 180)
datan x = 180 / pi * atan x

ld d = norm (280.4665 + 0.98564736 * d + 0.0003 * t * t) 360 -- ʿ�Ѳ��� l(d)
       where t = d / 36525
omegad d = 282.9373 + 0.00004708 * d + 0.0005 * t * t        -- ���������� omage(d)
       where t = d / 36525
ed d = 0.016709 - 0.000042 * t      -- Υ��Ψ e(d)
       where t = d / 36525

norm :: Float -> Integer -> Float   -- 0��360�٤�������
norm a d = fromInteger (b `mod` d) + c
   where (b,c) = properFraction a   -- b,c��a���������Ⱦ�����
lon td =                            -- td(JD)�ˤ��������۲��Ф�׻�
  norm l 360   -- td = julian date of the day time
   where d = td - 2451545.0         -- 2000ǯ1��1������(UT)���������
         e = ed d                   -- ���λ����Ǥ�Υ��Ψ
         omega = omegad d           -- ���λ����Ǥζ���������
         m = ld d - omega           -- ʿ�Ѷ���Υ��
         e' = kepler e m            -- Υ������Υ��
         kepler e m =               -- Kepler���������
           f e0
            where m' = m * pi / 180 -- ����ˡ��ľ��
                  e0 = m'           -- Newtonˡ�ν����
                  f e0 =
                   if abs (e0 - e1) < 0.00001 then (e1 * 180 / pi) else (f e1)
                     where e1 = e0 - (e0 - e * sin e0 - m')/(1 - e * cos e0)
         l = v e m + omega          -- ���۲���
           where v e m = 2 * datan (sqrt ((1 + e) / (1 - e)) * tand (e'/2))

