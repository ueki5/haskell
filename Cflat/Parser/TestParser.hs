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
    runTestTT test_int
    -- runTestTT testIO
    -- runTestTT test_form
    -- runTestTT test_form_invalid
    runTestTT test_form_left
    runTestTT test_form_right
    runTestTT test_form_lr

test_int = "test int" ~: test [ 
               "int 0" ~: (parser int "0")  ~?= Just (TpInt 0,"")
              ,"int 1" ~: (parser int "1")  ~?= Just (TpInt 1,"")
              ,"int 2" ~: (parser int "2")  ~?= Just (TpInt 2,"")
              ,"int 3" ~: (parser int "3")  ~?= Just (TpInt 3,"")
              ,"int 4" ~: (parser int "4")  ~?= Just (TpInt 4,"")
              ,"int 5" ~: (parser int "5")  ~?= Just (TpInt 5,"")
              ,"int 6" ~: (parser int "6")  ~?= Just (TpInt 6,"")
              ,"int 7" ~: (parser int "7")  ~?= Just (TpInt 7,"")
              ,"int 8" ~: (parser int "8")  ~?= Just (TpInt 8,"")
              ,"int 9" ~: (parser int "9")  ~?= Just (TpInt 9,"")
              ,"int 00" ~: (parser int "00")  ~?= Just (TpInt 0,"")
              ,"int 01" ~: (parser int "01")  ~?= Just (TpInt 1,"")
              ,"int 09" ~: (parser int "09")  ~?= Just (TpInt 9,"")
              ,"int 0A" ~: (parser int "0A")  ~?= Just (TpInt 0,"A")
              ,"int A0" ~: (parser int "A0")  ~?= Nothing
              ,"null test1" ~: null [] ~=? True
              -- ,"null test2" ~: null [] ~=? False
           ]

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

test_form = "test form" ~: test [ 
               "form 1+1" ~: (parser form "1+1")  ~?= Just (Op Plus (Tp (TpInt 1)) (Tp (TpInt 1)),"")
              ,"form 1 + 1" ~: (parser form "1 + 1")  ~?= Just (Op Plus (Tp (TpInt 1)) (Tp (TpInt 1)),"")
              ,"form 1-1" ~: (parser form "1-1")  ~?= Just (Op Minus (Tp (TpInt 1)) (Tp (TpInt 1)),"")
              ,"form 1*1" ~: (parser form "1*1")  ~?= Just (Op Mult (Tp (TpInt 1)) (Tp (TpInt 1)),"")
              ,"form 1/1" ~: (parser form "1/1")  ~?= Just (Op Div (Tp (TpInt 1)) (Tp (TpInt 1)),"")
              ,"form 1+1*1" ~: (parser form "1+1*1")  ~?= Just (Op Plus (Tp (TpInt 1)) (Op Mult (Tp (TpInt 1)) (Tp (TpInt 1))) , "")
              ,"form 1*1+1" ~: (parser form "1*1+1")  ~?= Just (Op Plus (Op Mult (Tp (TpInt 1)) (Tp (TpInt 1))) (Tp (TpInt 1)) , "")
              ,"form 1*1*1*1*1*1+1" ~: (parser form "1*1*1*1*1*1+1")  ~?= Just (Op Plus (Op Mult (Tp (TpInt 1)) (Op Mult (Tp (TpInt 1)) (Op Mult (Tp (TpInt 1)) (Op Mult (Tp (TpInt 1)) (Op Mult (Tp (TpInt 1)) (Tp (TpInt 1))))))) (Tp (TpInt 1)),"")
           ]
test_form_invalid = "test form invalid" ~: test [ 
               "form 1++1" ~: (parser form "1++1")  ~?= Nothing
              ,"form +1" ~: (parser form "+1")  ~?= Nothing
              ,"form D" ~: (parser form "D")  ~?= Nothing
              ,"form 1+" ~: (parser form "1+")  ~?= Nothing
              ,"form null" ~: (parser form "")  ~?= Nothing
           ]
test_form_left = "test form left" ~: test [ 
               "form left[+]" ~: (parser form "1+2")  ~?= Just (Op Plus (Tp (TpInt 1)) (Tp (TpInt 2)),"")
              ,"form left[++]" ~: (parser form "1+2+3")  ~?= Just (Op Plus (Op Plus (Tp (TpInt 1)) (Tp (TpInt 2))) (Tp (TpInt 3)),"")
              ,"form left[-]" ~: (parser form "1-2")  ~?= Just (Op Minus (Tp (TpInt 1)) (Tp (TpInt 2)),"")
              ,"form left[--]" ~: (parser form "1-2-3")  ~?= Just (Op Minus (Op Minus (Tp (TpInt 1)) (Tp (TpInt 2))) (Tp (TpInt 3)),"")
              ,"form left[+-]" ~: (parser form "1+2-3")  ~?= Just (Op Minus (Op Plus (Tp (TpInt 1)) (Tp (TpInt 2))) (Tp (TpInt 3)),"")
              ,"form left[-+]" ~: (parser form "1-2+3")  ~?= Just (Op Plus (Op Minus (Tp (TpInt 1)) (Tp (TpInt 2))) (Tp (TpInt 3)),"")
           ]
test_form_right = "test form right" ~: test [ 
               "form right[*]" ~: (parser form "1*2")  ~?= Just (Op Mult (Tp (TpInt 1)) (Tp (TpInt 2)),"")
              ,"form right[**]" ~: (parser form "1*2*3")  ~?= Just (Op Mult (Tp (TpInt 1)) (Op Mult (Tp (TpInt 2)) (Tp (TpInt 3))),"")
              ,"form right[/]" ~: (parser form "1/2")  ~?= Just (Op Div (Tp (TpInt 1)) (Tp (TpInt 2)),"")
              ,"form right[//]" ~: (parser form "1/2/3")  ~?= Just (Op Div (Tp (TpInt 1)) (Op Div (Tp (TpInt 2)) (Tp (TpInt 3))),"")
              ,"form right[*/]" ~: (parser form "1*2/3")  ~?= Just (Op Mult (Tp (TpInt 1)) (Op Div (Tp (TpInt 2)) (Tp (TpInt 3))),"")
              ,"form right[/*]" ~: (parser form "1/2*3")  ~?= Just (Op Div (Tp (TpInt 1)) (Op Mult (Tp (TpInt 2)) (Tp (TpInt 3))),"")
           ]
test_form_lr = "test form lr" ~: test [ 
               "form lr[+*]" ~: (parser form "1+2*3")  ~?= Just (Op Plus (Tp (TpInt 1)) (Op Mult (Tp (TpInt 2)) (Tp (TpInt 3))),"")
              ,"form lr[*+]" ~: (parser form "1*2+3")  ~?= Just (Op Plus (Op Mult (Tp (TpInt 1)) (Tp (TpInt 2))) (Tp (TpInt 3)),"")
              ,"form lr[++*]" ~: (parser form "1+2+3*4")  ~?= Just (Op Plus (Op Plus (Tp (TpInt 1)) (Tp (TpInt 2))) (Op Mult (Tp (TpInt 3)) (Tp (TpInt 4))),"")
              ,"form lr[+++*]" ~: (parser form "1+2+3+4*5")  ~?= Just (Op Plus (Op Plus (Op Plus (Tp (TpInt 1))  (Tp (TpInt 2))) (Tp (TpInt 3))) (Op Mult (Tp (TpInt 4)) (Tp (TpInt 5))),"")
           ]
