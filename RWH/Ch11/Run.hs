import Ch05.Prettify
import Test.QuickCheck.Batch

options = TestOptions
          {
            no_of_tests = 200
            , length_of_tests = 1
            , debug_tests = False
          }

main = do "simple" options
          [  run prop_empty_id
           , run prop_char
           , run prop_text
           , run prop_line
           , run prop_double ]
