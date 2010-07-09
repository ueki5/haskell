import Ch05.SimpleJSON as SimpleJSON
-- -- ‚i‚u‚‚Œ‚•‚…
-- data JValue = JString String
--             | JNumber Double
--             | JBool Bool
--             | JNull
--             | JObject [(String, JValue)]
--             | JArray [JValue]
--               deriving (Eq, Ord, Show)
-- getString :: JValue -> Maybe String
-- getString (JString s) = Just s
-- getString _ = Nothing
-- getInt :: JValue -> Maybe Int
-- getInt (JNumber d) = Just (truncate d)
-- getInt _ = Nothing
-- getDouble :: JValue -> Maybe Double
-- getDouble (JNumber d) = Just d
-- getDouble _ = Nothing
-- getBool :: JValue -> Maybe Bool
-- getBool (JBool b) = Just b
-- getBool _ = Nothing
-- getObject :: JValue -> Maybe [(String, JValue)]
-- getObject (JObject o) = Just o
-- getObject _ = Nothing
-- getArray :: JValue -> Maybe [JValue]
-- getArray (JArray a) = Just a
-- getArray _ = Nothing
-- isNull JNull = True
-- isNull _ = False
-- ‚i‚r‚n‚m
type JSONError = String
class JSON a where
    toJValue :: a -> JValue
    fromJValue :: JValue -> Either JSONError a
instance JSON JValue where
    toJValue = id
    fromJValue = Right
-- instance JSON Bool where
--     toJValue = JBool
--     fromJValue (JBool b) = Right b
--     fromJValue _ = Left "not a JSON boolean"