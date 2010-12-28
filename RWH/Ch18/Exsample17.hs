-- ����̓��O�G���g���̃t�H�[�}�b�g�ł�
import Ch18.Exsample18 hiding (logMsg,Entry,Log)
import Control.Monad.Writer
data Entry = Log {count::Int, msg::String} deriving Eq

-- ���b�Z�[�W�����O�ɒǉ�
logMsg :: String -> Writer [Entry] ()
logMsg s = tell [Log 1 s]

-- ����͈�̃p�P�b�g�������܂�
filterOne :: [Rule] -> Packet -> Writer [Entry] (Maybe Packet)
filterOne rules packet = do rule <- return (match rules packet)
                            case rule of
                              Nothing  -> do logMsg ("DROPPING UNMATCHED PACKET: " ++ (show packet))
                                             return Nothing
                              (Just r) -> do when (logIt r) (logMsg ("MATCH: " ++ (show r) ++ " <=> " ++ (show packet)))
                                             case r of
                                               (Rule Accept _ _) -> return (Just packet)
                                               (Rule Reject _ _) -> return Nothing
