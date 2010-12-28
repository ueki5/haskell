-- これはログエントリのフォーマットです
import Ch18.Exsample18 hiding (logMsg,Entry,Log)
import Control.Monad.Writer
data Entry = Log {count::Int, msg::String} deriving Eq

-- メッセージをログに追加
logMsg :: String -> Writer [Entry] ()
logMsg s = tell [Log 1 s]

-- これは一つのパケットを扱います
filterOne :: [Rule] -> Packet -> Writer [Entry] (Maybe Packet)
filterOne rules packet = do rule <- return (match rules packet)
                            case rule of
                              Nothing  -> do logMsg ("DROPPING UNMATCHED PACKET: " ++ (show packet))
                                             return Nothing
                              (Just r) -> do when (logIt r) (logMsg ("MATCH: " ++ (show r) ++ " <=> " ++ (show packet)))
                                             case r of
                                               (Rule Accept _ _) -> return (Just packet)
                                               (Rule Reject _ _) -> return Nothing
