import List
import Puzzle

type Sudoku = Int

instance Puzzle Sudoku where
  puzzleElm     = [1..sudokuSize]
  vacant        = 0
  candidate b p = puzzleElm \\ nub (concatMap (\ c -> c b p) [col,row,box])

sudokuBase, sudokuSize :: Int
sudokuBase = 3
sudokuSize = sudokuBase ^ 2

col, row, box :: Board Sudoku -> Position -> [Sudoku]
col b (i,_) = transpose b !! i
row b (_,j) = b !! j
box b (i,j) = concat 
            $ map (take sudokuBase) $ map (drop $ (i `div` sudokuBase) * sudokuBase)
            $ take sudokuBase $ drop ((j `div` sudokuBase) * sudokuBase) b

sudoku :: Board Sudoku -> [Board Sudoku]
sudoku = solve

showSudoku :: Board Sudoku -> String
showSudoku = showBoard
readSudoku :: String -> Board Sudoku
readSudoku = readBoard

instance Show (Board Sudoku) where
  show = showBoard 

instance Read (Board Sudoku) where
  readsPrec _ s = [(readSudoku s,"")]

sampledata :: String
sampledata = unlines 
           ["8 0 0 0 3 4 0 5 0"
           ,"0 0 2 0 0 0 0 0 1"
           ,"0 1 0 9 0 0 0 0 0"
           ,"0 0 8 0 0 9 0 0 6"
           ,"5 0 0 0 1 0 0 0 8"
           ,"6 0 0 4 0 0 7 0 0"
           ,"0 0 0 0 0 1 0 7 0"
           ,"2 0 0 0 0 0 1 0 0"
           ,"0 9 0 5 6 0 0 0 2"]

sample :: Board Sudoku
sample = read sampledata









































































































































