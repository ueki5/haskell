module Main () where
import Ch05.SimpleJSON
import Ch05.PutJSON

main = putJValue (JObject [("foo", JNumber 1), ("bar", JBool False)])