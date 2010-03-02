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

type Position = (Int,Int) -- (列,行)

sudoku :: SudokuBoard -> [SudokuBoard]
sudoku b
 = case vacantPositions b of
    [] -> [b]                            -- 空白のマス目が無い → 終了
    ps -> case nextVacant b ps of        -- 空白のマス目が有る
            (p,xs) ->                    -- 空白のマス目の位置 p と候補リスト xs
                      concatMap sudoku   -- 新しい盤面をそれぞれ解いて集める
                    $ map (putCell b)    -- 各ペアで更新した盤面を作る
                    $ [(p,x) | x <- xs]  -- 位置と候補のペアのリスト

vacantPositions :: SudokuBoard -> [Position]
nextVacant      :: SudokuBoard -> [Position] -> (Position,[Sudoku])
putCell         :: SudokuBoard -> (Position,Sudoku) -> SudokuBoard

vacantPositions = error "(vacantPositions) is not yet implemented"
nextVacant      = error "(nextVacant) is not yet implemented"
putCell         = error "(putCell) is not yet implemented"
