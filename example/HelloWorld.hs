import System.IO
main = do
  hSetEncoding stdout utf8
  hSetEncoding stdin utf8
  putStrLn "こんにちは　世界!!"
  str <- getLine
  putStrLn (addMsg str)
  otherMsg
addMsg::String -> String
addMsg msg = msg ++ "だそうです。"
otherMsg = do
  putStrLn "さようなら。"
-- import System.IO
-- main = do
--   h <- openFile filename WriteMode
--   hSetEncoding h utf8
--   hSetEncoding stdout utf8
--   hPutStrLn h "こんにちは　世界!!"
--   putStrLn "こんにちは　世界!!"
--   hClose h
-- filename = "hello.txt"

  
