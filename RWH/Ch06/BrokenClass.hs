{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverlappingInstances #-}
module Ch06.BrokenClass where
import Ch06.JSONclass
import Ch05.SimpleJSON
-- import Ch05.SimpleJSON
instance (JSON a) => JSON [a] where
    toJValue [] = JNull
    toJValue _ = JNull

    fromJValue _ = Left "i don't know"
instance (JSON a) => JSON [(String, a)] where
    toJValue _ = JNull
    fromJValue _ = Left "i don't know"
-- instance JSON JValue where
--     toJValue = id
--     fromJValue = Right
-- instance JSON Bool where
--     toJValue = JBool
--     fromJValue (JBool b) = Right b
--     fromJValue _ = Left "not a JSON boolean"
