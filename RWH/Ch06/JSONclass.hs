{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverlappingInstances #-}
{-# LANGUAGE UndecidableInstances #-}

module Ch06.JSONclass where
import Control.Arrow
import Ch05.SimpleJSON
-- ‚i‚r‚n‚m
newtype JAry a = JAry {
      fromJAry :: [a]
    } deriving (Eq, Ord, Show)
type JSONError = String
class JSON a where
    toJValue :: a -> JValue
    fromJValue :: JValue -> Either JSONError a
instance JSON JValue where
    toJValue = id
    fromJValue = Right
instance JSON String where
    toJValue = JString
    fromJValue (JString s) = Right s
    fromJValue _ = Left "not a JSON string"
instance JSON Double where
    toJValue = JNumber
    fromJValue (JNumber d) = Right d
    fromJValue _ = Left "not a JSON Num"
instance JSON Bool where
    toJValue = JBool
    fromJValue (JBool b) = Right b
    fromJValue _ = Left "not a JSON boolean"
instance (JSON a) => JSON [(String, a)] where
    toJValue [] = JNull
    toJValue ps = JObject $ map (second toJValue) ps
    fromJValue (JObject []) = Right []
    fromJValue (JObject ((name, value):ps)) = (name, fromJValue value) `objectcons` (fromJValue (JObject ps))
    fromJValue _ = Left "i don't know"
instance (JSON a) => JSON [a] where
    toJValue [] = JNull
    toJValue as = JArray $ map toJValue as
    fromJValue (JArray []) = Right []
    fromJValue (JArray (j:js)) = (fromJValue j) `eithercons` (fromJValue (JArray js))
    fromJValue _ = Left "not a JSON array"

eithers :: (a -> Either JSONError a) -> [a] -> Either JSONError [a]
eithers f [] = Right []
eithers f (a:as) = f a `eithercons` eithers f as
eithercons :: Either JSONError a -> Either JSONError [a] -> Either JSONError [a]
eithercons (Left s) _ = Left s
eithercons _ (Left s) = Left s
eithercons (Right a) (Right as) = Right (a:as)
objects :: (a -> Either JSONError a) -> [(String, a)] -> Either JSONError [(String, a)]
objects f [] = Right []
objects f ((name, value):ps) = (name, f value) `objectcons` objects f ps
objectcons :: (String, Either JSONError a) -> Either JSONError [(String, a)] -> Either JSONError [(String, a)]
objectcons (_, Left s) _ = Left s
objectcons _ (Left s) = Left s
objectcons (name, Right value) (Right []) = Right [(name, value)]
objectcons (name, Right value) (Right ps) = Right ((name, value):ps)

obj :: JValue
obj = toJValue [("a",0::Double),("b",1::Double),("c",2::Double),("d",3::Double)]
ary :: JValue
ary = toJValue [(0::Double),(1::Double),(2::Double),(3::Double)]
fromobj :: Either JSONError [(String, Double)]
fromobj = fromJValue obj
fromary :: Either JSONError [Double]
fromary = fromJValue ary
