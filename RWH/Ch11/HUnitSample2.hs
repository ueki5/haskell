import System.Directory
import Test.HUnit
import Control.Exception
createEmptyFile file = writeFile file ""

testIO = test [   "is file created?" ~: 
                  (do 
                    createEmptyFile file
                    doesFileExist file )  ~? "is file created?"
                , "is create file empty?" ~: 
                  (do 
                    createEmptyFile file
                    txt <- readFile file
                    return $ null txt )  ~? "is create file empty?"
              ]
    where file = "test.txt"
testIO' = "create empty file" ~: 
          (do 
            createEmptyFile file
            exi <- doesFileExist file 
            assertBool "createEmptyFile create file." exi
            txt <- readFile (file ++ "wk")
            assertEqual "created file is empty." txt "")
    where file = "test.txt"
testIO'' = "create empty file" ~: 
        (finally
          (do 
            (doesFileExist file >>= return . not) 
                @? "Pre-condition test: File already exist."
            createEmptyFile file
            doesFileExist file @? "createEmptyFile create file."
            txt <- readFile file
            "" @=? txt)
           (removeFile file))
    where file = "test.txt"
testIO''' = "create empty file" ~: 
        (bracket_
          (do 
            (doesFileExist file >>= return . not) 
                @? "Pre-condition test: File already exist.")
          (do
            removeFile file
            (doesFileExist file >>= return . not) 
                @? "Post-condition test: File already exist."))
          (do 
            createEmptyFile file
            doesFileExist file 
                @? "createEmptyFile create file."
            txt <- readFile file
            "" @=? txt)
    where file = "test.txt"
