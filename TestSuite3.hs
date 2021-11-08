{-# LANGUAGE ScopedTypeVariables #-}
module TestSuite3 where

import Test.Tasty
import Test.Tasty.HUnit

import Control.Exception
import Test.Tasty.HUnit

import Angabe3


-- Das hier nach der `module ... where` Anweisung
-- Das hier irgendwo im code.
expectError :: a -> String -> Assertion
expectError val msg = do
  res <- try (evaluate val)
  case res of
    Left (ErrorCall err) -> err @?= msg
    Right _ -> assertFailure "Expected error call"


spec :: TestTree
spec = testGroup "Exercise 3" [a_1, a_2, a_3,a_4_mat_add,a_4_mat_neg,a_4_mat_mult, a_4_mat_abs, a_4_mat_signum, a_4_mat_fromInteger]

a_1 :: TestTree
a_1 =
  testGroup
    "Matrix show tests"
    [ testCase "0 - show (M [[1,2],[3,4]]) ->> \"([1,2] [3,4])\"" $
        "([1,2] [3,4])" @?= show (M [[1,2],[3,4]]),
      testCase "1 - show (M [[1,2],[3,4],[5,6]]) ->> \"([1,2] [3,4] [5,6])\"" $
        "([1,2] [3,4] [5,6])" @?= show (M [[1,2],[3,4],[5,6]]),
      testCase "2 - show (M [[1,2,3],[4,5],[6]]) ->> \"([1,2,3] [4,5] [6])\"" $
        "([1,2,3] [4,5] [6])" @?= show (M [[1,2,3],[4,5],[6]]),
      testCase "3 - show (M [[1,2,3],[],[6]]) ->> \"([1,2,3] [] [6])\"" $
        "([1,2,3] [] [6])" @?= show (M [[1,2,3],[],[6]]),
      testCase "4 - show (M [[],[],[]]) ->> \"([] [] [])\"" $
        "([] [] [])" @?= show (M [[],[],[]]),
      testCase "5 - show (M []) ->> \"()\"" $
        "()" @?= show (M [])
    ]

a_2 :: TestTree
a_2 =
  testGroup
    "matrixtyp test"
    [ testCase "0 - matrixtyp (M [[1,2],[3,4]]) ->> Mat (2,2)" $
        (Mat (2,2)) @?= matrixtyp (M [[1,2],[3,4]]),
      testCase "1 - matrixtyp (M [[1,2],[3,4],[5,6]]) ->> Mat (3,2)" $
        (Mat (3,2)) @?= matrixtyp (M [[1,2],[3,4],[5,6]]),
      testCase "2 - matrixtyp (M [[1,2,3],[4,5],[6]]) ->> KeineMatrix" $
        KeineMatrix @?= matrixtyp (M [[1,2,3],[4,5],[6]]),
      testCase "3 - matrixtyp (M [[1,2,3],[],[6]]) ->> KeineMatrix" $
        KeineMatrix @?= matrixtyp (M [[1,2,3],[],[6]]),
      testCase "4 - matrixtyp (M [[],[],[]]) ->> KeineMatrix" $
        KeineMatrix @?= matrixtyp (M [[],[],[]]),
      testCase "5 - matrixtyp (M []) ->> KeineMatrix" $
        KeineMatrix @?= matrixtyp (M [])
    ]

a_3 :: TestTree
a_3 =
  testGroup
    "matrix eq test"
    [ testCase "0 - (M [[1,2],[3,4]]) == (M [[1,2],[3,4]]) ->> True" $
        True @?= ((M [[1,2],[3,4]]) == (M [[1,2],[3,4]])),
      testCase "1 - (M [[1,2],[3,4]]) == (M [[1,2],[3,5]]) ->> False" $
        False @?= (M [[1,2],[3,4]]) == (M [[1,2],[3,5]]),
      testCase "2 - (M [[1,2],[3,4]]) /= (M [[1,2],[3,4]]) ->> False" $
        False @?= ((M [[1,2],[3,4]]) /= (M [[1,2],[3,4]])),
      testCase "3 - (M [[1,2],[3,4]]) /= (M [[1,2],[3,5]]) ->> True" $
        True @?= (M [[1,2],[3,4]]) /= (M [[1,2],[3,5]]),
      testCase "4 - (M [[1,2],[3,4]]) == (M [[1,2],[3,4],[5,6]) ->> False" $
        False @?= (M [[1,2],[3,4]]) == (M [[1,2],[3,4],[5,6]]),
      testCase "5 - (M []) == (M []) ->> error \"Gleichheit undefiniert\"" $
        expectError ((M []) == (M []))("Gleichheit undefiniert"),
      testCase "6 - (M []) /= (M []) ->> error \"Ungleichheit undefiniert\"" $
        expectError ((M []) /= (M []))("Ungleichheit undefiniert")
    ]

a_4_mat_add :: TestTree
a_4_mat_add =
  testGroup
    "Matrix Addition and Substraction"
    [ testCase "0 - (M [[3,4,2]]) + (M [[13,9,7,15],[8,7,4,6],[6,4,0,3]]) ->> ()" $
        show (M []) @?= show ((M [[3,4,2]]) + (M [[13,9,7,15],[8,7,4,6],[6,4,0,3]])),
      testCase "1 - (M [[1,3],[2,4]]) + (M [[5,7],[6,8]]) ->> ([6,10] [8,12])" $
        show (M [[6,10],[8,12]]) @?= show ((M [[1,3],[2,4]]) + (M [[5,7],[6,8]])),
      testCase "2 - (M [[1,3]]) + (M [[5,9]]) ->> ([6,12])" $
        show (M [[6,12]]) @?= show ((M [[1,3]]) + (M [[5,9]])),
      testCase "2 - (M [[5,8]]) - (M [[3,5]]) ->> ([2,3])" $
        show (M [[2,3]]) @?= show ((M [[5,8]]) - (M [[3,5]]))
    ]

a_4_mat_neg :: TestTree
a_4_mat_neg =
  testGroup
    "Matrix negation"
    [ testCase "0 - negate (M [[3,0,-2]]) ->> ([-3,0,2])" $
        (M [[-3,0,2]]) @?= negate (M [[3,0,-2]])
   ]

a_4_mat_mult :: TestTree
a_4_mat_mult =
  testGroup
    "Matrix multiplication"
    [ testCase "0 - (M [[3,4,2]]) * (M [[13,9,7,15],[8,7,4,6],[6,4,0,3]]) ->> ([83,63,37,75])" $
        (M [[83,63,37,75]]) @?= (M [[3,4,2]]) * (M [[13,9,7,15],[8,7,4,6],[6,4,0,3]]),
      testCase "1 - (M [[1,3],[2,4]]) * (M [[5,7],[6,8]]) ->> ([23,31] [34,46])" $
        (M [[23,31],[34,46]]) @?= (M [[1,3],[2,4]]) * (M [[5,7],[6,8]]),
      testCase "2 - (M [[1,3]]) * (M [[5]]) ->> ()" $
        show (M []) @?= show ((M [[1,3]]) * (M [[5]]))
    ]

a_4_mat_abs :: TestTree
a_4_mat_abs =
  testGroup
    "Matrix absolute value"
    [ testCase "0 - abs (M [[3,0,-2]]) ->> ([3,0,2])" $
        (M [[3,0,2]]) @?= abs (M [[3,0,-2]])
   ]

a_4_mat_signum :: TestTree
a_4_mat_signum =
  testGroup
    "Matrix signum"
    [ testCase "0 - signum (M [[3,2,1]]) ->> ([1])" $
        (M [[1]]) @?= signum (M [[3,2,1]]),
      testCase "1 - signum (M [[0,0,0]]) ->> ([0])" $
        (M [[0]]) @?= signum (M [[0,0,0]]),
      testCase "2 - signum (M [[-3,-2,-1]]) ->> ([-1])" $
        (M [[-1]]) @?= signum (M [[-3,-2,-1]]),
      testCase "3 - signum (M []) ->> error \"Vorzeichenfunktion undefiniert\"" $
        expectError (signum (M []))("Vorzeichenfunktion undefiniert"),
      testCase "4 - signum (M [[1,0,-1]]) ->> error \"Vorzeichenfunktion undefiniert\"" $
        expectError (signum (M [[1,0,-1]]))("Vorzeichenfunktion undefiniert")
 
        
   ]

a_4_mat_fromInteger :: TestTree
a_4_mat_fromInteger =
  testGroup
    "Matrix from integer"
    [ testCase "0 - fromInteger 42 :: Matrix ->> ([42])" $
        (M [[42]]) @?= (fromInteger 42 :: Matrix)
   ]

