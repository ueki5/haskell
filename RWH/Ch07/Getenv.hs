module Ch07.Getenv where
import System.Environment(getEnv)
main = do
  strhome <- getEnv "HOME"
  putStrLn strhome