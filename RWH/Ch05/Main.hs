module Main where
import RWH.Ch05.SimpleJSON
import RWH.Ch05.PutJSON

main = putJValue (JObject [("foo", JNumber 1), ("bar", JBool False)])