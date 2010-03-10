beep::IO ()
beep = putStr "\BEL"
cls::IO ()
cls = putStr "\ESC[2J"
main = do beep
          cls