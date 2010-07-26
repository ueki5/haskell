import Text.Regex.Posix
main = do
  ret <- return ("your right hand" =~ "bar" :: Bool)
  putStrLn $ show ret
  ret <- return ("I,Buu. Ionsanii, uurit a lift'd batch" =~ "(uu|ii)" :: String)
  putStrLn $ show ret
  return ()
