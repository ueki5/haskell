module Cflat.Parser.TestParser where
import Cflat.Parser.Parser
import Cflat.Type.Type
import Test.HUnit
import Control.Monad.State
import Control.Exception
import System.Directory
import System.Environment

main = do
    getProgName >>= print
    -- runTestTT test_int
    -- runTestTT testIO

-- test_int = "test int" ~: test [ 
--                "int 0" ~: (parser int "0")  ~?= Just (TpInt 0,"")
--               ,"int 1" ~: (parser int "1")  ~?= Just (TpInt 1,"")
--               ,"int 2" ~: (parser int "2")  ~?= Just (TpInt 2,"")
--               ,"int 3" ~: (parser int "3")  ~?= Just (TpInt 3,"")
--               ,"int 4" ~: (parser int "4")  ~?= Just (TpInt 4,"")
--               ,"int 5" ~: (parser int "5")  ~?= Just (TpInt 5,"")
--               ,"int 6" ~: (parser int "6")  ~?= Just (TpInt 6,"")
--               ,"int 7" ~: (parser int "7")  ~?= Just (TpInt 7,"")
--               ,"int 8" ~: (parser int "8")  ~?= Just (TpInt 8,"")
--               ,"int 9" ~: (parser int "9")  ~?= Just (TpInt 9,"")
--               ,"int 00" ~: (parser int "00")  ~?= Just (TpInt 0,"")
--               ,"int 01" ~: (parser int "01")  ~?= Just (TpInt 1,"")
--               ,"int 09" ~: (parser int "09")  ~?= Just (TpInt 9,"")
--               ,"int 0A" ~: (parser int "0A")  ~?= Just (TpInt 0,"A")
--               ,"int A0" ~: (parser int "A0")  ~?= Nothing
--               ,"null test1" ~: null [] ~=? True
--               -- ,"null test2" ~: null [] ~=? False
--            ]

createEmptyFile file = writeFile file "this is temp file"
testIO = "createEmptyFile" ~:
         (do 
           (bracket
             (return ())
             -- (\dmy  -> return ())
             (\dmy  -> removeFile file)
             (\dmy' -> (do
                 (doesFileExist file >>= return . not) @? "Pre-condition test: File already exist."
                 createEmptyFile file
                 exi <- doesFileExist file
                 exi @? "file is not exists."
                 txt <- readFile file
                 txt @=? "this is temp file")))
           (doesFileExist file >>= \ret -> return . not $ ret) @? "Post-condition test: file is not removed.")
       where file = "sample.txt"
test_imp = "import" ~: test [ 
               "importStmt" ~: (parser importStmt "Import aaa.bbb")  ~?= Join "aaa" "." "bbb"
              -- ,"int 1" ~: (parser int "1")  ~?= Just (TpInt 1,"")
              -- ,"null test1" ~: null [] ~=? True
           ]
