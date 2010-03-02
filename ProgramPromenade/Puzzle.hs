module Puzzle where

import List

type Board a = [[a]]       -- ����
type Position = (Int,Int)  -- �ޥ��ܰ���

putCell :: Board a -> (Position, a) -> Board a
putCell b ((i,j),x) = ls0 ++ (cs0 ++ (x:cs1)):ls1
		      where (ls0,l:ls1) = splitAt j b
                            (cs0,_:cs1) = splitAt i l

class Eq a => Puzzle a where
  puzzleElm  :: [a]                        -- �ޥ��ܤ�������Υꥹ��
  vacant     :: a                          -- �ޥ��ܤ˲������äƤ��ʤ����Ȥ�ɽ�����
  candidate  :: Board a -> Position -> [a] -- ���ꤷ���ޥ��ܤΰ��֤�������Τθ���(�ꥹ��)

solve :: Puzzle a => Board a -> [Board a]
solve b = case vacantPositions b of
            [] -> [b]
            ps -> case nextVacant b ps of
                    (p,xs) -> concatMap solve
			    $ map (putCell b)
                            $ [ (p,x) | x <- xs ]

vacantPositions :: Puzzle a => Board a -> [Position]
vacantPositions b = map fst
                     $ filter (\ (p,x) -> vacant == x)
                     $ concatMap (\ (j,xs) -> zipWith (\ i x -> ((i,j),x)) [0..] xs )
                     $ zip [0..] b

nextVacant :: Puzzle a => Board a -> [Position] -> (Position,[a])
nextVacant b ps = minimumBy cmp [ (p, candidate b p) | p <- ps ]
   where 
     (_,xs) `cmp` (_,ys) = length xs `compare` length ys

showBoard :: (Puzzle a, Show a) => Board a -> String
showBoard = unlines . map (unwords . map show)

readBoard :: (Puzzle a, Read a) => String -> Board a
readBoard = map (map read) . map words . lines
