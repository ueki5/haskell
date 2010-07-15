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
instance JSON Bool where
    toJValue = JBool
    fromJValue (JBool b) = Right b
    fromJValue _ = Left "not a JSON boolean"
