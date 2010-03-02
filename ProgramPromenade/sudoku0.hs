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

vacantPositions = error "(vacantPositions) is not yet implemented"
nextVacant      = error "(nextVacant) is not yet implemented"
putCell         = error "(putCell) is not yet implemented"
