module Ch08.SumFile where
main = do
  contents <- getContents
  print (sumFile contents)
    where sumFile = sum . map read . words