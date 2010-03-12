import System.Environment

lineCount [] = 0
lineCount (c:[]) = 1
lineCount ('\n':cs) = 1 + lineCount cs
lineCount (c:cs) = lineCount cs
wordCount str = wordCountImpl str False
    where wordCountImpl [] False = 0
          wordCountImpl [] True = 1
          wordCountImpl (' ':cs) False = wordCountImpl cs False
          wordCountImpl (' ':cs) True = 1 + wordCountImpl cs False
          wordCountImpl ('\t':cs) False = wordCountImpl cs False
          wordCountImpl ('\t':cs) True = 1 + wordCountImpl cs False
          wordCountImpl ('\n':cs) False = wordCountImpl cs False
          wordCountImpl ('\n':cs) True = 1 + wordCountImpl cs False
          wordCountImpl (c:cs) inWord = wordCountImpl cs True
byteCount [] = 0
byteCount (c:cs) = 1 + byteCount cs

wcFile fileName =
    do contents <- readFile fileName
       putStrLn ("\t" ++ show (lineCount contents)
                 ++ "\t" ++ show (wordCount contents)
                 ++ "\t" ++ show (byteCount contents)
                 ++ "\t" ++ fileName)
main = do args <- getArgs
          mapM_ wcFile args
