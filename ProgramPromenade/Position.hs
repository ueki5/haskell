module Position where
       -- �G�N�X�|�[�g���X�g�𗪂��ƁC���ׂĂ̒l�C�^�C�N���X���G�N�X�|�[�g�����

import Mark		( Mark(..), isEmpty, next )
import Data.List	( transpose )

-- Positon(�ǖ�)  ���̎�ԂƔՖʂ���Ȃ�
-- Table(�Ֆ�)    ���X�g�̃��X�g�œ񎟌��̔Ֆʂ��`�D
--		  �����̊e���X�g�̒����͓��������̂Ƃ���D
data Position = Pos { turn :: Mark, table :: Table }
type Table = [[Mark]]

mkPos :: Int -> Int -> Position		-- row1 x col1 �̔Ֆʂ� U �Ŗ��߂��ǖ�
mkPos row1 col1 = Pos O (replicate row1 (replicate col1 U))

gameLimit :: Int
gameLimit = 3			-- n �ڕ��ׂ� n

instance Show Position where
  show (Pos p pss) = "---------> " ++ show p ++
		     concatMap showLine pss ++ "\t\t"
    where 
      showLine cs = "\n" ++ unwords (map show cs)


getCell :: (Int,Int) -> Table -> Mark
getCell (x,y) pss
  | x<0 || row<x || y<0 || col<y  = error "Illegal move"
  | otherwise                     = pss !! x !! y
  where (row,col) = row_col pss

putCell :: (Int,Int) -> Mark -> Table -> Table
putCell (x,y) p pss
  | x<0 || row<x || y<0 || col<y  = error "Illegal move"
  | not $ isEmpty p0              = error "Not empty"
  | otherwise                     = pss'
  where
    (row,col) = row_col pss
    (pss0, ps:pss1) = splitAt x pss
    (ps0,  p0:ps1)  = splitAt y ps
    ps'  = ps0  ++ p:ps1
    pss' = pss0 ++ ps':pss1

-- ���X�g�̃C���f�b�N�X(0����n�܂�)�̏���Ƃ��ėp����̂Ń}�C�i�X 1 ���Ă���
row_col tab = (length tab - 1, length (head tab) - 1)

updatePosition :: (Int,Int) -> Position -> Position
updatePosition (x,y) (Pos p pss) = Pos (next p) (putCell (x,y) p pss)


allLines :: Table -> [[Mark]]
allLines tab = [ line | line <- rows++cols++ diags, length line >= gameLimit ]
  where
    rows  = tab
    cols  = transpose tab
    diags = [ [ getCell (i,j) tab | i<-[0..row], let j=k+i, 0<=j && j<=col ]
              | k <- [-row..col] ] ++
            [ [ getCell (i,j) tab | i<-[0..row], let j=k-i, 0<=j && j<=col ]
              | k <- [0..row+col] ]
    (row,col) = row_col tab

allCells :: Table -> [Mark]
allCells tab = concat tab


-- �ǖ� pos �ɑ΂��C���̂��ׂĂ̋ǖʌ��̃��X�g��Ԃ�
allMoves :: Position -> [Position]
allMoves pos@(Pos _ pss)
  | winlosegame pos || drawgame pos = [] 
  | otherwise = [ updatePosition (x,y) pos |
		  x <- [0 .. row],
                  y <- [0 .. col],
		  isEmpty (getCell (x,y) pss) ]
  where (row,col) = row_col pss

winlosegame :: Position -> Bool
winlosegame (Pos p pss) = or [ fin line | line <- allLines pss ]
  where
    fin :: [Mark] -> Bool
    fin qs = fin' 0 qs		-- qs �̒��� p' �� n ����ł���� True
      where
        fin' n [] = False
	fin' n (q:qs)
	  | q == p'   = let n1 = n+1 in 
			if n1 >= gameLimit then True else fin' n1 qs
          | otherwise = fin' 0 qs
    p' = next p

drawgame :: Position -> Bool
drawgame (Pos _ pss)
  = and [ not $ isEmpty cell | cell <- allCells pss ]
