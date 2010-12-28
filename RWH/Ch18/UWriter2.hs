import Control.Monad.Writer

-- メッセージをログに追加
logMsg :: Int -> Writer [(Int, String)] ()
logMsg n = tell [(n, show n)]

-- これは一つのパケットを扱います
-- filterOne :: Int -> Writer [Entry] (Maybe Int)
-- filterOne n =  case n of
--                  0 -> do logMsg ("DROPPING UNMATCHED PACKET: " ++ (show n))
--                       return Writer Nothing
--                  1 -> do when (logIt n) (logMsg ("MATCH: " ++ (show n) ++ " <=> " ++ (show n)))
--                       case n of
--                         0 -> return (Just n)
--                         1 -> return Nothing
