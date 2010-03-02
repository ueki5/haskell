import List

sudokuBase, sudokuSize :: Int
sudokuBase = 3
sudokuSize = sudokuBase ^ 2

type Sudoku = Int

sudokuElm :: [Sudoku]
sudokuElm = [1..sudokuSize]

vacant :: Sudoku
vacant = 0

type SudokuBoard = [[Sudoku]]

sample :: SudokuBoard
sample = [[8,0,0,0,3,4,0,5,0]
         ,[0,0,2,0,0,0,0,0,1]
         ,[0,1,0,9,0,0,0,0,0]
         ,[0,0,8,0,0,9,0,0,6]
         ,[5,0,0,0,1,0,0,0,8]
         ,[6,0,0,4,0,0,7,0,0]
         ,[0,0,0,0,0,1,0,7,0]
         ,[2,0,0,0,0,0,1,0,0]
         ,[0,9,0,5,6,0,0,0,2]]

type Position = (Int,Int) -- (��,��)

sudoku :: SudokuBoard -> [SudokuBoard]
sudoku b
 = case vacantPositions b of
    [] -> [b]                            -- ����Υޥ��ܤ�̵�� �� ��λ
    ps -> case nextVacant b ps of        -- ����Υޥ��ܤ�ͭ��
            (p,xs) ->                    -- ����Υޥ��ܤΰ��� p �ȸ���ꥹ�� xs
                      concatMap sudoku   -- ���������̤򤽤줾��򤤤ƽ����
                    $ map (putCell b)    -- �ƥڥ��ǹ����������̤���
                    $ [(p,x) | x <- xs]  -- ���֤ȸ���Υڥ��Υꥹ��

vacantPositions :: SudokuBoard -> [Position]
nextVacant      :: SudokuBoard -> [Position] -> (Position,[Sudoku])
putCell         :: SudokuBoard -> (Position,Sudoku) -> SudokuBoard

putCell b ((i,j), x) = ls0++(xs0++x:xs1):ls1
                       where 
                         (ls0,l:ls1) = splitAt j b
                         (xs0,_:xs1) = splitAt i l

nextVacant b ps = minimumBy cmp [(p,candidate b p) | p <- ps]
  where
    (_,xs) `cmp` (_,ys) = length xs `compare` length ys

candidate :: SudokuBoard -> Position -> [Sudoku]
candidate b p = sudokuElm \\ nub (concat $ map (\ c -> c b p) [col,row,box])

col, row, box :: SudokuBoard -> Position -> [Sudoku]
col b (i,_) = transpose b !! i
row b (_,j) = b !! j
box b (i,j) = concat
            $ map (take sudokuBase) $ map (drop $ (i `div` sudokuBase) * sudokuBase)
            $ take sudokuBase $ drop ((j `div` sudokuBase) * sudokuBase) b

vacantPositions b = map fst
                     $ filter (\ (p,x) -> vacant == x)
                     $ concatMap (\ (j,xs) -> zipWith (\ i x -> ((i,j),x)) [0..] xs )
                     $ zip [0..] b

showSudokuBoard :: SudokuBoard -> String
showSudokuBoard b = unlines $ map (unwords . map show) b

readSudokuBoard :: String -> SudokuBoard
readSudokuBoard s = map (map read) $ map words $ lines s

instance Show SudokuBoard where
  show = showSudokuBoard

instance Read SudokuBoard where
  readsPrec _ str = [(readSudokuBoard str, "")]
