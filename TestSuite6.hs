module TestSuite6 where

import Test.Tasty
import Test.Tasty.HUnit

import Control.Exception
import Test.Tasty.HUnit

import Angabe6

-- Hier Test Gruppen hinzufuegen
-- Add test groups here
spec :: TestTree
spec = testGroup "Exercise 6" [a_1, a_2, a_4, a_5]

-- Das hier nach der `module ... where` Anweisung
-- Das hier irgendwo im code.
expectError :: a -> String -> Assertion
expectError val msg = do
  res <- try (evaluate val)
  case res of
    Left (ErrorCall err) -> err @?= msg
    Right _ -> assertFailure "Expected error call"

-- Testdaten:
m1 = Mf (2,2) (\z s -> if z == 1 then s else z+s) :: MatrixF
m2 = Mf (2,2) (\z s -> s + ((z-1)*(snd (2,2)))) :: MatrixF
m3 = Mf (2,2) (\z s -> s + ((z-1)*(snd (mtyp m2)))) :: MatrixF
m4 = Mf (2,2) (\z s -> if z == 1 then (succ (fib (s-1)))
                                      else ((+) z (binom z (s-1)))) :: MatrixF

m5 = Mf (3,2) (\z s -> if z == 1 then s
                       else if z == 2 then z+s
                       else succ (z+s)) :: MatrixF
m6 = Mf (3,2) (\z s -> z * s + (z-1) * (mod s 2))  :: MatrixF

-- Ungueltige matrizen
m7 = Mf (0,0) (\_ _ -> 0) :: MatrixF
m8 = Mf (0,0) (\z s -> z + s) :: MatrixF
m9 = Mf (0,1) (\_ _ -> 0) :: MatrixF
m10 = Mf (1,0) (\_ _ -> 0) :: MatrixF

-- Andere
m11 = Mf (2,2) (\_ _ -> 0) :: MatrixF
m12 = Mf (1,3) (\z s -> if (s == 1) then 1
                        else if (s == 2) then 0
                        else -1) :: MatrixF
m13 = Mf (2,3) (\s z -> z * s + (z-1) * (mod s 2))  :: MatrixF

-- Fuer Multiplikation
m14 = Mf (2,3) (\z s -> if z == 1 then s else z+s) :: MatrixF
m15 = Mf (3,2) (\z s -> if z == 1 then s else z+s) :: MatrixF
m16 = Mf (4,2) (\z s -> if z == 1 then s else z+s) :: MatrixF

-- Hilfsfunktionen
fib :: Nat0 -> Nat0
fib 0 = 0
fib 1 = 1
fib n = fib (n-2) + fib (n-1)

binom :: Nat0 -> Nat0 -> Nat1
binom n k
  | k <= 0 || n == k = 1
  | True             = binom (n-1) (k-1) + binom (n-1) k



a_1 :: TestTree
a_1 =
  testGroup
    "a_1"
    [ testCase "show m1 ->> \"([1,2] [3,4])\"" $
        show m1 @?= "([1,2] [3,4])",
      testCase "show m2 ->> \"([1,2] [3,4])\"" $
        show m2 @?= "([1,2] [3,4])",
      testCase "show m3 ->> \"([1,2] [3,4])\"" $
        show m3 @?= "([1,2] [3,4])",
      testCase "show m4 ->> \"([1,2] [3,4])\"" $
        show m4 @?= "([1,2] [3,4])",
      testCase "show m5 ->> \"([1,2] [3,4] [5,6])\"" $
        show m5 @?= "([1,2] [3,4] [5,6])",
      testCase "show m6 ->> \"([1,2] [3,4] [5,6])\"" $
        show m6 @?= "([1,2] [3,4] [5,6])",
      testCase "show m6 ->> \"()\"" $
        show m7 @?= "()",
      testCase "show m6 ->> \"()\"" $
        show m8 @?= "()"
    ]

a_2 :: TestTree
a_2 =
  testGroup
    "a_2"
    [ testCase "matrixtyp m1 ->> Just (2,2)" $
        matrixtyp m1 @?= Just (2,2),
      testCase "matrixtyp m2 ->> Just (2,2)" $
        matrixtyp m2 @?= Just (2,2),
      testCase "matrixtyp m3 ->> Just (2,2)" $
        matrixtyp m3 @?= Just (2,2),
      testCase "matrixtyp m4 ->> Just (2,2)" $
        matrixtyp m4 @?= Just (2,2),
      testCase "matrixtyp m5 ->> Just (3,2)" $
        matrixtyp m5 @?= Just (3,2),
      testCase "matrixtyp m6 ->> Just (3,2)" $
        matrixtyp m6 @?= Just (3,2),
      testCase "matrixtyp m7 ->> Nothing" $
        matrixtyp m7 @?= Nothing,
      testCase "matrixtyp m8 ->> Nothing" $
        matrixtyp m8 @?= Nothing,
      testCase "matrixtyp m9 ->> Nothing" $
        matrixtyp m9 @?= Nothing,
      testCase "matrixtyp m10 ->> Nothing" $
        matrixtyp m10 @?= Nothing 
 
    ]


      --testCase "sind_gleich int_m8 int_m6 ->> error Fehler" $
       -- expectError (sind_gleich int_m8 int_m6) ("Fehler"),
 

a_4 :: TestTree
a_4 =
  testGroup
    "a_4"
    [ testCase "m1 == m1 ->> True" $
        m1 == m1 @?= True,
      testCase "m1 /= m1 ->> False" $
        m1 /= m1 @?= False,
      testCase "m1 == m2 ->> True" $
        m1 == m2 @?= True,
      testCase "m1 == m4 ->> True" $
        m1 == m4 @?= True,
      testCase "m1 /= m2 ->> False" $
        m1 /= m2 @?= False,
      testCase "m1 == m5 ->> False" $
        m1 == m5 @?= False,
      testCase "m1 /= m5 ->> True" $
        m1 /= m5 @?= True,
      testCase "m5 == m5 ->> True" $
        m5 == m5 @?= True,
      testCase "m1 == m7 ->> error \"Gleichheit undefiniert\"" $
        expectError (m1 == m7) ("Gleichheit undefiniert"),
      testCase "m7 == m1 ->> error \"Gleichheit undefiniert\"" $
        expectError (m7 == m1) ("Gleichheit undefiniert"),
      testCase "m1 /= m7 ->> error \"Ungleichheit undefiniert\"" $
        expectError (m1 /= m7) ("Ungleichheit undefiniert"),
      testCase "m7 /= m1 ->> error \"Ungleichheit undefiniert\"" $
        expectError (m1 /= m7) ("Ungleichheit undefiniert"),
      testCase "m9 == m7 ->> error \"Gleichheit undefiniert\"" $
        expectError (m9 == m7) ("Gleichheit undefiniert")
    ]


a_5 :: TestTree
a_5 =
  testGroup
    "a_5"
    [ testCase "m1 + m1 ->> ([2,4] [6,8])" $
        show (m1 + m1) @?= "([2,4] [6,8])", 
      testCase "m1 + m4 ->> ([2,4] [6,8])" $
        show (m1 + m4) @?= "([2,4] [6,8])", 
      testCase "m6 + m5 ->> ([2,4] [6,8] [10,12])" $
        show (m6 + m5) @?= "([2,4] [6,8] [10,12])", 
      testCase "m1 + m5 ->> ()" $
        show (m1 + m5) @?= "()", 
      testCase "m7 + m1 ->> ()" $
        show (m7 + m1) @?= "()", 
      testCase "m1 - m1 ->> ([0,0] [0,0])" $
        show (m1 - m1) @?= "([0,0] [0,0])", 
      testCase "m5 - m6 ->> ([0,0] [0,0] [0,0])" $
        show (m5 - m6) @?= "([0,0] [0,0] [0,0])",
      testCase "m1 - m5 ->> ()" $
        show (m1 - m5) @?= "()", 
      testCase "m7 - m1 ->> ()" $
        show (m7 + m1) @?= "()", 
      testCase "negate m5 ->> ([-1,-2] [-3,-4] [-5,-6])" $
        show (negate m5) @?= "([-1,-2] [-3,-4] [-5,-6])",
      testCase "negate m7 ->> ()" $
        show (negate m7) @?= "()",
      testCase "abs (negate m1) ->> ([1,2] [3,4])" $
        show (abs (negate m1)) @?= "([1,2] [3,4])",
      testCase "abs (negate m7) ->> ()" $
        show (abs (negate m7)) @?= "()",
      testCase "fromInteger 1 ->> ([1])" $
        show (fromInteger 1 :: MatrixF) @?= "([1])",
      testCase "fromInteger -5 ->> ([-5])" $
        show (fromInteger (-5) :: MatrixF) @?= "([-5])",
      testCase "signum m1 ->> ([1])" $
        show (signum m1) @?= "([1])",
      testCase "signum (negate m1) ->> ([-1])" $
        show (signum (negate m1)) @?= ([-1]),
      testCase "signum m11 ->> ([0])" $
        show (signum m11) @?= ([0]),
      testCase "signum m12 ->> error Vorzeichen undefiniert" $
        expectError (signum m12) ("Vorzeichenfunktion undefiniert"),
      testCase "signum m7 ->> error Vorzeichen undefiniert" $
        expectError (signum m7) ("Vorzeichenfunktion undefiniert"),
      testCase "m1 * m1 ->> ([7,10] [15,22])" $
        show (m1 * m1) @?= "([7,10] [15,22])",
      testCase "m1 * m3 ->> ([7,10] [15,22])" $
        show (m1 * m3) @?= "([7,10] [15,22])",        
      testCase "m6 * m13 ->> ([5,11,17] [11,25,39] [17,39,61])" $
        show (m6 * m13) @?= "([5,11,17] [11,25,39] [17,39,61])",
      testCase "m14 * m15 ->> ([19,25] [35,47])" $
        show (m14 * m15) @?= "([19,25] [35,47])", 
      testCase "m14 * m16 ->> ()" $
        show (m14 * m16) @?= "()",
      testCase "m1 * m7 ->> ()" $
        show (m1 * m7) @?= "()"
    ]





