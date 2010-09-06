module Main where
import Test.HUnit 
import Control.Monad.State

test1 = test [ runState (return 12 >>= put) 11 ~?= runState (put 12) 11
               , runState (get >>= return) 12 ~?= runState get 12
               , runState (get >>= (\x -> put x >>= return)) 12
                     ~?= runState (get >>= put >>= return) 12 ]
test1' = test [ "test1'-1" ~: runState (return 12 >>= put) 11 ~?= runState (put 11) 11
               , "test1'-2" ~: runState (get >>= return) 12 ~?= runState get 12
               , "test1'-3" ~: runState (get >>= (\x -> put x >>= return)) 12
                     ~?= runState (get >>= put >>= return) 12 ]
test2 = test [  runState (put 12) 11 ~=? runState (return 12 >>= put) 11
               , runState get 12 ~=? runState (get >>= return) 12
               , runState (get >>= put >>= return) 12
                     ~=? runState (get >>= (\x -> put x >>= return)) 12 ]
test3 = test [ "test3-1" ~: null [] ~? "Is List null?"]
test4 = test [ "test4-1" ~: null [1] ~? "Is List not null?"
               , "test4-2" ~: null [1] ~? "Is List not null too!?"]

test5 = "Monad Laws" ~:
        test [ "return a >>= k  ==  k a" ~: 
               runState (return 1 >>= put) 11  ~?= runState (put 12) 11
             , "m >>= return ==  m" ~: 
               runState (get >>= return) 12 ~?= runState get 12
             , "m >>= (\\x -> k x >>= h)  ==  (m >>= k) >>= h" ~:  
               runState (get >>= (\x -> put x >>= return)) 12
                   ~?= runState (get >>= put >>= return) 12 ]
