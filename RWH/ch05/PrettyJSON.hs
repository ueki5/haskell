module PrettyJSON
    (
     renderJValue
    ) where
import SimpleJSON(JValue(..))
import Prettify
    (
     Doc(..)
     ,(<>)
     ,string
     ,text
     ,double
     ,series
    )
------------------------------------------------------------
renderJValue :: JValue -> Doc
renderJValue (JString s) = string s
renderJValue (JNumber n) = double n
renderJValue (JBool True) = text "true"
renderJValue (JBool False) = text "false"
renderJValue JNull = text "null"
renderJValue (JArray ary) = series '[' ']' renderJValue ary
renderJValue (JObject obj) = series '{' '}' field obj
    where field :: (String, JValue) -> Doc
          field (name, val) = string name
                      <> text ": "
                      <> renderJValue val
-- putJValue :: JValue -> IO ()
-- putJValue v = putStrLn (renderJValue v)