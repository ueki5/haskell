module RWH.Ch05.PutJSON where

import Data.List (intercalate)
import RWH.Ch05.SimpleJSON

renderJValue :: JValue -> String
renderJValue (JString s) = show s
renderJValue (JNumber n) = show n
renderJValue (JBool True) = "true"
renderJValue (JBool False) = "false"
renderJValue JNull = "null"

renderJValue (JObject o) = "{" ++ pairs o ++ "}"
    where pairs [] = ""
          pairs ps = intercalate "," (map renderpairs ps)
              where renderpairs (k, v) = "(" ++ k ++ "," ++ (renderJValue v) ++ ")"
renderJValue (JArray a) = "[" ++ values a ++ "]"
    where values [] = []
          values vs = intercalate "," (map renderJValue vs)
putJValue :: JValue -> IO ()
putJValue v = putStrLn (renderJValue v)
