module Ch07.Filter where
main = interact (unlines . filter (elem 'a') . lines)