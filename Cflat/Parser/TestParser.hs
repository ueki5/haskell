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
    runTestTT testIO
    -- runTestTT test_parse_file

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
-- test_imp = "import" ~: test [ 
--                "importStmt" ~: (parser importStmt "Import aaa.bbb")  ~?= Join "aaa" "." "bbb"
--               ,"int 1" ~: (parser int "1")  ~?= Just (TpInt 1,"")
--               ,"null test1" ~: null [] ~=? True
--            ]
-- source = "\nimport stdio;\nimport stdio;\n"
test_parse_file = "parse from file" ~: test [ 
               "add" ~: (parseFile "../test/add.cb")  ~?= Nothing
           ]
-- parseFile "../test/addressof.cb"
-- parseFile "../test/alloca.cb"
-- parseFile "../test/alloca2.cb"
-- parseFile "../test/aref-semcheck.cb"
-- parseFile "../test/aref-semcheck2.cb"
-- parseFile "../test/array-semcheck1.cb"
-- parseFile "../test/array.cb"
-- parseFile "../test/array2.cb"
-- parseFile "../test/assign.cb"
-- parseFile "../test/assoc.cb"
-- parseFile "../test/bitand.cb"
-- parseFile "../test/bitnot.cb"
-- parseFile "../test/bitor.cb"
-- parseFile "../test/bitxor.cb"
-- parseFile "../test/block.cb"
-- parseFile "../test/break-semcheck.cb"
-- parseFile "../test/cast.cb"
-- parseFile "../test/cast2.cb"
-- parseFile "../test/charops.cb"
-- parseFile "../test/charops2.cb"
-- parseFile "../test/comm.cb"
-- parseFile "../test/condexpr.cb"
-- parseFile "../test/const.cb"
-- parseFile "../test/continue-semcheck.cb"
-- parseFile "../test/dec.cb"
-- parseFile "../test/decloverride.cb"
-- parseFile "../test/decloverride2.cb"
-- parseFile "../test/defun-semcheck.cb"
-- parseFile "../test/defun-semcheck2.cb"
-- parseFile "../test/defun-semcheck3.cb"
-- parseFile "../test/defun-semcheck4.cb"
-- parseFile "../test/defun-semcheck5.cb"
-- parseFile "../test/defun-semcheck6.cb"
-- parseFile "../test/defun-semcheck7.cb"
-- parseFile "../test/defun-semcheck8.cb"
-- parseFile "../test/defvar.cb"
-- parseFile "../test/deref-semcheck1.cb"
-- parseFile "../test/deref-semcheck2.cb"
-- parseFile "../test/deref-semcheck3.cb"
-- parseFile "../test/deref-semcheck4.cb"
-- parseFile "../test/deref-semcheck5.cb"
-- parseFile "../test/div.cb"
-- parseFile "../test/dowhile-break.cb"
-- parseFile "../test/dowhile-continue.cb"
-- parseFile "../test/dowhile1.cb"
-- parseFile "../test/dowhile2.cb"
-- parseFile "../test/dowhile3.cb"
-- parseFile "../test/duplicated-import.cb"
-- parseFile "../test/empstruct.cb"
-- parseFile "../test/eq.cb"
-- parseFile "../test/for-break.cb"
-- parseFile "../test/for-continue.cb"
-- parseFile "../test/for1.cb"
-- parseFile "../test/fork.cb"
-- parseFile "../test/funcall-semcheck.cb"
-- parseFile "../test/funcall-semcheck2.cb"
-- parseFile "../test/funcall0.cb"
-- parseFile "../test/funcall1.cb"
-- parseFile "../test/funcall2.cb"
-- parseFile "../test/funcall3.cb"
-- parseFile "../test/funcall4.cb"
-- parseFile "../test/funcall5.cb"
-- parseFile "../test/funcptr.cb"
-- parseFile "../test/gt.cb"
-- parseFile "../test/gteq.cb"
-- parseFile "../test/gvar.cb"
-- parseFile "../test/hello.cb"
-- parseFile "../test/hello2.cb"
-- parseFile "../test/hello3.cb"
-- parseFile "../test/hello4.cb"
-- parseFile "../test/if1.cb"
-- parseFile "../test/if2.cb"
-- parseFile "../test/implicitaddr.cb"
-- parseFile "../test/inc.cb"
-- parseFile "../test/initializer.cb"
-- parseFile "../test/integer.cb"
-- parseFile "../test/intops.cb"
-- parseFile "../test/invalidstmt1.cb"
-- parseFile "../test/invalidstmt2.cb"
-- parseFile "../test/logicaland.cb"
-- parseFile "../test/logicalnot.cb"
-- parseFile "../test/logicalor.cb"
-- parseFile "../test/longops.cb"
-- parseFile "../test/lshift.cb"
-- parseFile "../test/lt.cb"
-- parseFile "../test/lteq.cb"
-- parseFile "../test/lvar1.cb"
-- parseFile "../test/lvar2.cb"
-- parseFile "../test/mdarray.cb"
-- parseFile "../test/mdarray2.cb"
-- parseFile "../test/mod.cb"
-- parseFile "../test/mul.cb"
-- parseFile "../test/neq.cb"
-- parseFile "../test/noreturn.cb"
-- parseFile "../test/one.cb"
-- parseFile "../test/opassign.cb"
-- parseFile "../test/param.cb"
-- parseFile "../test/pointer.cb"
-- parseFile "../test/pointer2.cb"
-- parseFile "../test/pointer3.cb"
-- parseFile "../test/pointer4.cb"
-- parseFile "../test/ptrarray.cb"
-- parseFile "../test/ptrdiff.cb"
-- parseFile "../test/ptrmemb.cb"
-- parseFile "../test/ptrmemb2.cb"
-- parseFile "../test/recursivetypedef.cb"
-- parseFile "../test/rshift.cb"
-- parseFile "../test/scomm.cb"
-- parseFile "../test/setjmptest.cb"
-- parseFile "../test/sgvar.cb"
-- parseFile "../test/shortops.cb"
-- parseFile "../test/shortops2.cb"
-- parseFile "../test/sizeof-expr.cb"
-- parseFile "../test/sizeof-struct.cb"
-- parseFile "../test/sizeof-type.cb"
-- parseFile "../test/sizeof-union.cb"
-- parseFile "../test/slcomm.cb"
-- parseFile "../test/slvar.cb"
-- parseFile "../test/src1.cb"
-- parseFile "../test/src2.cb"
-- parseFile "../test/staticfunc.cb"
-- parseFile "../test/string.cb"
-- parseFile "../test/struct-semcheck.cb"
-- parseFile "../test/struct-semcheck10.cb"
-- parseFile "../test/struct-semcheck2.cb"
-- parseFile "../test/struct-semcheck3.cb"
-- parseFile "../test/struct-semcheck4.cb"
-- parseFile "../test/struct-semcheck5.cb"
-- parseFile "../test/struct-semcheck6.cb"
-- parseFile "../test/struct-semcheck7.cb"
-- parseFile "../test/struct-semcheck8.cb"
-- parseFile "../test/struct-semcheck9.cb"
-- parseFile "../test/struct.cb"
-- parseFile "../test/struct2.cb"
-- parseFile "../test/struct3.cb"
-- parseFile "../test/sub.cb"
-- parseFile "../test/switch.cb"
-- parseFile "../test/syntax1.cb"
-- parseFile "../test/syntax2.cb"
-- parseFile "../test/syntax3.cb"
-- parseFile "../test/textwrite.cb"
-- parseFile "../test/ucharops.cb"
-- parseFile "../test/ucharops2.cb"
-- parseFile "../test/uintops.cb"
-- parseFile "../test/ulongops.cb"
-- parseFile "../test/unaryminus.cb"
-- parseFile "../test/unaryplus.cb"
-- parseFile "../test/union-semcheck.cb"
-- parseFile "../test/union-semcheck10.cb"
-- parseFile "../test/union-semcheck2.cb"
-- parseFile "../test/union-semcheck3.cb"
-- parseFile "../test/union-semcheck4.cb"
-- parseFile "../test/union-semcheck5.cb"
-- parseFile "../test/union-semcheck6.cb"
-- parseFile "../test/union-semcheck7.cb"
-- parseFile "../test/union-semcheck8.cb"
-- parseFile "../test/union-semcheck9.cb"
-- parseFile "../test/union.cb"
-- parseFile "../test/usertype.cb"
-- parseFile "../test/ushortops.cb"
-- parseFile "../test/ushortops2.cb"
-- parseFile "../test/utf.cb"
-- parseFile "../test/validstmt1.cb"
-- parseFile "../test/var-semcheck.cb"
-- parseFile "../test/varargs.cb"
-- parseFile "../test/vardecl.cb"
-- parseFile "../test/while-break.cb"
-- parseFile "../test/while-continue.cb"
-- parseFile "../test/while1.cb"
-- parseFile "../test/while2.cb"
-- parseFile "../test/while3.cb"
-- parseFile "../test/zero.cb"
