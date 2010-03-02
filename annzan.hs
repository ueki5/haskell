jimport System.Random

type Anzan = (Int, Int)

isfinished :: Masu -> Bool
isFinished [] = True
isFinished _ = False

-- markBoard :: Board -> Pt -> [Pt]
-- markBoard board pt@(x, y) = loop pt []
--     where ball = board !! x !! y 
--           loop pt@(x, y) marks 
--               | isValidPos board pt && board !! x !! y == ball = 
--                   foldr loop (pt:marks) $ 
--                         filter (flip notElem marks) [(x+1, y),(x-1, y),(x, y+1),(x, y-1)] 
--               | otherwise = marks

-- pickBall :: Board -> [Pt] -> Board
-- pickBall board marks = filter ((/=0).length) $zipWith (\x column -> snd $ unzip $ filter (\ (y, ball) -> notElem (x, y) marks) $ zip [0..] column) [0..] board

-- digits = [0..10]

-- mkRandomMasu :: Int -> Int -> Int -> IO Board
-- mkRandomMasu x y cols =
--     do seed <- getStdGen
--        rs <- return $ randomRs (0, cols - 1) seed
--        return $ take x $ mkTable y $ map (digits!!) $ rs
--     where mkTable n list = column : mkTable n rest
--             where (column, rest) = splitAt n list

-- maxX = 450
-- maxY = 300

-- calcScore marks = (length marks - 2) * (length marks - 2)

-- samegame board = do print board
--                     x <- getLine >>= return . read
--                     y <- getLine >>= return . read
--                     newBoard <- return $ pickBall board $ markBoard board (x, y)
--                     if isFinished newBoard
--                        then print newBoard
--                        else samegame newBoard

-- main = do board <- mkRandomBoard 3 3 3
--           samegame board
