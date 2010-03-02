import System.Random
-- import Graphics.HGL

-- type Ball = Color
data Ball = Red | Blue | Yellow | Green | Magenta | Cyan | White -- deriving (Eq,Show)
instance Show Ball where
  show Red = "R"
  show Blue = "B"
  show Yellow = "Y"
  show Green = "G"
  show Magenta = "M"
  show Cyan = "C"
  show White = "W"
instance Eq Ball where
  Red == Red = True
  Blue == Blue = True
  Yellow == Yellow = True
  Green == Green = True
  Magenta == Magenta = True
  Cyan == Cyan = True
  White == White = True
  _ == _ = False

type Pt = (Int, Int)
type Board = [[Ball]]

isValidPos :: Board -> Pt -> Bool
isValidPos [] _ = False
isValidPos (column:_) (0, y) = y >= 0 && y < length column
isValidPos (_ :columns) (x, y) = isValidPos columns (x-1, y)

-- isFinished :: Board -> Bool
-- isFinished board = and $ concat $ zipWith (\x column -> zipWith (\y _-> (length (markBoard board (x,y)) <= 1)) [0..] column) [0..] board
isFinished :: Board -> Bool
isFinished [] = True
isFinished _ = False

markBoard :: Board -> Pt -> [Pt]
markBoard board pt@(x, y) = loop pt []
    where ball = board !! x !! y 
          loop pt@(x, y) marks 
              | isValidPos board pt && board !! x !! y == ball 
                  = foldr loop (pt:marks) 
                    $ filter (flip notElem marks) [(x+1, y),(x-1, y),(x, y+1),(x, y-1)] 
              | otherwise = marks

pickBall :: Board -> [Pt] -> Board
pickBall board marks = filter ((/=0).length)
                          $ zipWith (\x column -> snd
                                    $ unzip 
                                    $ filter (\ (y, ball) -> notElem (x, y) marks)
                                    $ zip [0..] column) [0..] board

colors = [Red, Blue, Yellow, Green, Magenta, Cyan, White]

mkRandomBoard :: Int -> Int -> Int -> IO Board
mkRandomBoard x y cols =
    do seed <- getStdGen
       rs <- return $ randomRs (0, cols-1) seed
       return $ take x $ mkTable y $ map (colors!!) $ rs
    where mkTable n list = column : mkTable n rest
            where (column, rest) = splitAt n list

-- drawBoard :: Board -> Graphic
-- drawBoard baord = 
--     overGraphics $ concat $ zipWith (\x column -> zipWith (\y ball -> drawBall x y ball) [0..] column) [0..] board
--                  where drawBall x y ball = withColor ball $ ellipse (x*30, maxY-(y+1)*30) ((x+1)*30, maxY-y*30)

maxX = 450
maxY = 300

calcScore marks = (length marks - 2) * (length marks - 2)

-- samegame w board score = 
--     do drawInWindow w $ drawBoard baord
--        (px, py) <- getLBP w
--        x <- return $ div px 30
--        y <- return $ div (maxY - py) 30
--        marks <- return $ markBoard baord (x, y)
--        if length marks <= 1
--           then samegame w board score
--           else do newBoard <- return $ pickBall bard marks
--                   newScore <- return $ score + calcScore marks
--                   if isFinished newBoard
--                      then return newScore
--                      else do clearWindow w 
--                              samegame w newBoard newScore

-- main = runGraphics $
--        do board <- mkRandomBoard 15 10 3
--           score <- withWindow "samegame" (maxX maxY)
--                        (\w -> samegame w board 0)
--           putStrLn $ "Your Score is: " ++ show score

-- samegame board = do print board
--                           x <- getLine >>= return . read
--                           y <- getLine >>= return . read
--                           newBoard <- return $ pickBall baord (x, y)
--                           samegame newBoard

printBoard x@[] = print x
printBoard (x:[]) = print x
printBoard (x:xs) = do print x
                       printBoard xs

samegame board = do printBoard board
                    x <- getLine >>= return . read
                    y <- getLine >>= return . read
                    newBoard <- return $ pickBall board $ markBoard board (x, y)
                    if isFinished newBoard
                       then printBoard newBoard
                       else samegame newBoard

main = do board <- mkRandomBoard 3 3 3
          samegame board
