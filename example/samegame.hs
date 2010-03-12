import System.Random
import Graphics.HGL

type Ball = Color
type Pt = (Int, Int)
type Board = [[Ball]]

isValidPos :: Board -> Pt -> Bool
isValidPos [] _ = False
isValidPos (column:_) (0, y) = y >= 0 && y < length column
isValidPos (_ :column) (x, y) = isValidPos columns (x-1, y)

isFinished :: Board -> Bool
isFinished board = and $ concat $ zipWith (\x column -> zipWith (\y _-> (length (markBoard board (x,y)) <= 1)) [0..] column) [0..] board

markBoard :: Board -> Pt -> [Pt]
markBoard board pt@(x, y) = loop pt []
    where ball = board !! x !! y
                 loop pt@(x, y) marks
                      | is ValidPos board pt && board !! x !! y == ball = foldr loop (pt:marks) $ filter (flip notElem marks) [(x+1, y),(x-1, y),(x, y+1),(x, y-1)]
                      | otherwise = marks

pickBall :: Board -> [Pt] -> Board
pickBall baord marks = filter ((/=0).length) $zipWith (\x column -> snd $ unzip $ filter (\ (y, ball) -> notElem (x, y) marks) $ zip [0..] column) [0..] board

testBoard = [[Red, Red, Red, Blue, Blue]
             ,[Blue, Red, Red, Yellow, Blue]
             ,[Blue, Yellow, Yellow, Blue, Blue]
             ,[Yellow, Yellow, Red, Red, Yellow]
             ,[Red, Blue, Blue, Red, Yellow]]

colors = [Red, Blue, Yellow, Green, Magenta, Cyan, White]

mkRandomBoard :: Int -> Int -> Int -> IO Board
mkRandomBoard x y cols =
    do seed <- getStdGen
       rs <- return $ randomRs (0, cols-1) seed
       return $ take x $ mkTable y $ map (colors!!) $ rs
    where mkTable n list = column : mkTable n rest
            where (column, rest) = splitAt n list

drawBoard :: Board -> Graphic
drawBoard baord = 
    overGraphics $ concat $ zipWith (\x column -> zipWith (\y ball -> drawBall x y ball) [0..] column) [0..] board
                 where drawBall x y ball = withColor ball $ ellipse (x*30, maxY-(y+1)*30) ((x+1)*30, maxY-y*30)

maxX = 450
maxY = 300

calcScore marks = (length marks - 2) * (length marks - 2)

samegame w board score = 
    do drawInWindow w $ drawBoard baord
       (px, py) <- getLBP w
       x <- return $ div px 30
       y <- return $ div (maxY - py) 30
       marks <- return $ markBoard baord (x, y)
       if length marks <= 1
          then samegame w board score
          else do newBoard <- return $ pickBall bard marks
                  newScore <- return $ score + calcScore marks
                  if isFinished newBoard
                     then return newScore
                     else do clearWindow w 
                             samegame w newBoard newScore

main = runGraphics $
       do board <- mkRandomBoard 15 10 3
          score <- withWindow "samegame" (maxX maxY)
                       (\w -> samegame w board 0)
          putStrLn $ "Your Score is: " ++ show score