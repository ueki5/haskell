{-# LANGUAGE MultiParamTypeClasses#-}
{-# LANGUAGE FunctionalDependencies#-}
-- file: ch18/Reader.hs
class (Monad m) => MonadReader r m | m -> r where
    ask   :: m r
    local :: (r -> r) -> m a -> m a
