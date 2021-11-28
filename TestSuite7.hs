module TestSuite7 where

import Test.Tasty
import Test.Tasty.HUnit

import Angabe7

spec :: TestTree
spec = testGroup "Exercise 7" [a_2, a_3, a_6]

-- Matrix Instanzen --
ungueltig1 = M [[0, 1, 2, 4], [91, 42, 43, 8], [], [1]] :: Matrix
ungueltig2 = M [[20, 2], [1, 3, -3], [14, 0, 2, 3]] :: Matrix

m1_1 = M [[1]] :: Matrix
m1_2 = M [[5]] :: Matrix
m1_3 = M [[-3]] :: Matrix
m3x3 = M [[5, 10, 15], [3, 6, 9], [1, 2, 3]] :: Matrix
m3x3_x2 = M [[10, 20, 30], [6, 12, 18], [2, 4, 6]] :: Matrix
m3x3_zero = M [[0, 0, 0], [0, 0, 0], [0, 0, 0]] :: Matrix

tm1 = M [[5]] :: Matrix
tm2 = M [[1, 2], [3, 4]] :: Matrix
tm2_res = M [[1, 3],[2, 4]] :: Matrix
tm3 = M [[1, 2, 3], [4, 5, 6], [7, 8, 9]] :: Matrix
tm3_res = M [[1, 4, 7],[2, 5, 8], [3, 6, 9]] :: Matrix
tm4 = M [[1, 2]] :: Matrix
tm4_res = M [[1], [2]] :: Matrix
tm5 = M [[1], [2], [3], [4], [5]] :: Matrix
tm5_res = M [[1, 2, 3, 4, 5]] :: Matrix

dm4x4 = M [[1, 3, 5, 9], [1, 3, 1, 7], [4, 3, 9, 7], [5, 2, 0, 9]] :: Matrix
dm5x5 = M [[4, 6, 1, -1, 6], [5, 0, 1, 0, 2], [-5, 5, 6, 3, 8], [4, -1, 7, 9, 1], [2, 2, 3, 1, 4]] :: Matrix

-- MatrixF Instanzen --

-- Testdaten:
fm1 = Mf (2,2) (\z s -> if z == 1 then s else z+s) :: MatrixF
fm2 = Mf (2,2) (\z s -> s + ((z-1)*(snd (2,2)))) :: MatrixF
fm3 = Mf (2,2) (\z s -> s + ((z-1)*(snd (mtyp fm2)))) :: MatrixF
fm4 = Mf (2,2) (\z s -> if z == 1 then (succ (fib (s-1)))
                                      else ((+) z (binom z (s-1)))) :: MatrixF

fm5 = Mf (3,2) (\z s -> if z == 1 then s
                       else if z == 2 then z+s
                       else succ (z+s)) :: MatrixF
fm6 = Mf (3,2) (\z s -> z * s + (z-1) * (mod s 2))  :: MatrixF

-- Ungueltige matrizen
fm7 = Mf (0,0) (\_ _ -> 0) :: MatrixF
fm8 = Mf (0,0) (\z s -> z + s) :: MatrixF
fm9 = Mf (0,1) (\_ _ -> 0) :: MatrixF
fm10 = Mf (1,0) (\_ _ -> 0) :: MatrixF

-- Andere
fm11 = Mf (2,2) (\_ _ -> 0) :: MatrixF
fm12 = Mf (1,3) (\z s -> if (s == 1) then 1
                        else if (s == 2) then 0
                        else -1) :: MatrixF
fm13 = Mf (2,3) (\s z -> z * s + (z-1) * (mod s 2))  :: MatrixF

-- Fuer Multiplikation
fm14 = Mf (2,3) (\z s -> if z == 1 then s else z+s) :: MatrixF
fm15 = Mf (3,2) (\z s -> if z == 1 then s else z+s) :: MatrixF
fm16 = Mf (4,2) (\z s -> if z == 1 then s else z+s) :: MatrixF

fm17 = Mf (1,1) (\z s -> if z == 1 then 7 else z+s) :: MatrixF

fm18 = Mf (3,3) (\z s -> if z == 1 then s else z+s) :: MatrixF
fm19 = Mf (4,4) (\z s -> if z == 1 then s else z+s) :: MatrixF
fm20 = Mf (5,5) (\z s -> if z == 1 then s else z+s) :: MatrixF

fm1_konv = M ([[1, 2], [3, 4]]) :: Matrix
fm5_konv = M ([[1, 2], [3, 4], [5, 6]]) :: Matrix
fm17_konv = M ([[7]]) :: Matrix
fm18_konv = M ([[1, 2, 3], [3, 4, 5], [4, 5, 6]]) :: Matrix
fm19_konv = M ([[1, 2, 3, 4], [3, 4, 5, 6], [4, 5, 6, 7], [5, 6, 7, 8]]) :: Matrix
fm20_konv = M ([[1, 2, 3, 4, 5], [3, 4, 5, 6, 7], [4, 5, 6, 7, 8], [5, 6, 7, 8, 9] , [6, 7, 8, 9, 10]]) :: Matrix

fm1_transp = M ([[1, 3], [2, 4]]) :: Matrix
fm5_transp = M ([[1, 3, 5], [2, 4, 6]]) :: Matrix
fm14_transp = M ([[1, 3], [2, 4], [3, 5]]) :: Matrix
fm15_transp = M ([[1, 3, 4], [2, 4, 5]]) :: Matrix
fm16_transp = M ([[1, 3, 4, 5], [2, 4, 5, 6]]) :: Matrix

fm1_mult3 = M ([[3, 6], [9, 12]]) :: Matrix
fm5_multMinus2 = M ([[-2, -4], [-6, -8], [-10, -12]]) :: Matrix
fm14_mult4 = M ([[4, 8, 12], [12, 16, 20]]) :: Matrix
fm18_mult0 = M [[0, 0, 0], [0, 0, 0], [0, 0, 0]] :: Matrix
fm19_multMinus1 = M ([[-1, -2, -3, -4], [-3, -4, -5, -6], [-4, -5, -6, -7], [-5, -6, -7, -8]]) :: Matrix

-- VLADIMIR :-)
fp1 = [(AZ Viertel XIX,"A","B",RD II Schlag), (AZ Dreiviertel XVIII, "A", "B", RD I Schlag), (AZ Viertel XIX, "A", "B", RD XIII Schlag), (AZ Schlag XX, "B", "C", RD X Schlag), (AZ Schlag XVIII, "B", "C", RD XII Schlag), (AZ Schlag XVIII, "B", "C", RD XIII Schlag)]


fp2 = [(AZ Schlag XIX, "A", "B", RD II Schlag)] -- 0 konserven von A -> B

fp3 =  [(AZ Schlag XIX, "A", "B", RD I Schlag), (AZ Viertel XXI, "B", "C", RD I Schlag)] -- 1 konserve von A -> C


fp4 =  [(AZ Schlag XIX, "A", "B", RD I Schlag), (AZ Schlag XXI, "B", "A", RD I Schlag), (AZ Schlag XVIII, "A", "B", RD I Schlag), (AZ Viertel XXI, "B", "C", RD I Schlag),(AZ Halb XXI, "B", "C", RD I Schlag),(AZ Schlag XXI, "B", "C", RD I Schlag),(AZ Viertel XXII, "B", "C", RD I Schlag)] -- 0 konserve von A -> C 

fp5 = [
  (AZ Schlag XVIII, "A", "G", RD XII Viertel),
  (AZ Schlag XVIII, "A", "B", RD I Schlag), 
  (AZ Schlag XIX, "A", "C", RD II Schlag),
  (AZ Halb XIX, "A", "D", RD I Halb), 
  
  (AZ Schlag XVIII, "B", "F", RD I Schlag),

  (AZ Dreiviertel XXIII, "C", "E", RD II Viertel),
  
  (AZ Viertel II, "E", "F", RD I Schlag),
  (AZ Halb III, "E", "I", RD III Halb),
  (AZ Dreiviertel III, "E", "G", RD V Schlag),

  (AZ Schlag III, "F", "G", RD III Schlag),
  (AZ Schlag XVIII, "F", "H", RD IV Schlag),

  (AZ Schlag XIV, "C", "A", RD I Halb)
  ] -- A -> B -> F -> G; A -> C -> F -> G; both 1 | A -> ... -> F -> H; both 2




-- Hilfsfunktionen
fib :: Nat0 -> Nat0
fib 0 = 0
fib 1 = 1
fib n = fib (n-2) + fib (n-1)

binom :: Nat0 -> Nat0 -> Nat1
binom n k
  | k <= 0 || n == k = 1
  | True             = binom (n-1) (k-1) + binom (n-1) k


a_2 :: TestTree
a_2 =
  testGroup
    "a_2"
    [
        testCase "msmult 1 m_1 ->> m1_1" $
            msmult 1 m1_1 @?= m1_1,
        testCase "msmult 5 m_1 ->> m1_2" $
            msmult 5 m1_1 @?= m1_2,
        testCase "msmult (-3) m_1 ->> m1_3" $
            msmult (-3) m1_1 @?= m1_3,
        testCase "msmult 2 m3x3 ->> m3x3_x2" $
            msmult 2 m3x3 @?= m3x3_x2,
        testCase "msmult 0 m3x3 ->> m3x3_zero" $
            msmult 0 m3x3 @?= m3x3_zero,
        testCase "msmult 6 ungueltig1 ->> ()" $
            show (msmult 6 ungueltig1) @?= "()",
        testCase "msmult 3 ungueltig2 ->> ()" $
            show (msmult 3 ungueltig2) @?= "()",
        testCase "msmult 3 fm7 ->> ()" $
            show (msmult 3 fm7) @?= "()",
        testCase "msmult 4 fm8 ->> ()" $
            show (msmult 4 fm8) @?= "()",
        testCase "msmult (-2) fm9 ->> ()" $
            show (msmult (-2) fm9) @?= "()",
        testCase "msmult 3 fm1 ->> fm1_mult3" $
            konv2 (msmult 3 fm1) @?=  fm1_mult3,
        testCase "msmult (-2) fm5 ->> fm5_multMinus2" $
            konv2 (msmult (-2) fm5) @?=  fm5_multMinus2,
        testCase "msmult 4 fm14 ->> fm14_mult4" $
            konv2 (msmult 4 fm14) @?=  fm14_mult4,
        testCase "msmult 0 fm18 ->> fm18_mult0" $
            konv2 (msmult 0 fm18) @?=  fm18_mult0,
        testCase "msmult (-1) fm19 ->> fm19_multMinus1" $
            konv2 (msmult (-1) fm19) @?=  fm19_multMinus1,
        testCase "mtransp tm1 ->> tm1" $
            mtransp tm1 @?= tm1,
        testCase "mtransp tm2 ->> tm2_res" $
            mtransp tm2 @?= tm2_res,
        testCase "mtransp tm3 ->> tm3_res" $
            mtransp tm3 @?= tm3_res,
        testCase "mtransp tm4 ->> tm4_res" $
            mtransp tm4 @?= tm4_res,
        testCase "mtransp tm5 ->> tm5_res" $
            mtransp tm5 @?= tm5_res,
        testCase "mtransp ungueltig1 ->> ()" $
            show (mtransp ungueltig1) @?= "()",
        testCase "mtransp ungueltig2 ->> ()" $
            show (mtransp ungueltig2) @?= "()",
        testCase "mtransp fm1 ->> fm1_transp" $
            konv2 (mtransp fm1) @?= fm1_transp,
        testCase "mtransp fm2 ->> fm1_transp" $
            konv2 (mtransp fm2) @?= fm1_transp,
        testCase "mtransp fm3 ->> fm1_transp" $
            konv2 (mtransp fm3) @?= fm1_transp,
        testCase "mtransp fm4 ->> fm1_transp" $
            konv2 (mtransp fm4) @?= fm1_transp,
        testCase "mtransp fm5 ->> fm5_transp" $
            konv2 (mtransp fm5) @?= fm5_transp,
        testCase "mtransp fm6 ->> fm5_transp" $
            konv2 (mtransp fm6) @?= fm5_transp,
        testCase "mtransp fm14 ->> fm14_transp" $
            konv2 (mtransp fm14) @?= fm14_transp,
        testCase "mtransp fm15 ->> fm15_transp" $
            konv2 (mtransp fm15) @?= fm15_transp,
        testCase "mtransp fm16 ->> fm16_transp" $
            konv2 (mtransp fm16) @?= fm16_transp,       
        testCase "mtransp fm17 ->> fm17" $
            mtransp fm17 @?= fm17,
        testCase "mtransp fm7 ->> ()" $
            show (mtransp fm7) @?= "()",
        testCase "mtransp fm8 ->> ()" $
            show (mtransp fm8) @?= "()",
        testCase "mtransp fm9 ->> ()" $
            show (mtransp fm9) @?= "()",
        testCase "mtransp fm10 ->> ()" $
            show (mtransp fm10) @?= "()",
        testCase "mdet tm1 ->> Just 5" $
            mdet tm1 @?= Just 5,
        testCase "mdet tm2 ->> Just (-2)" $
            mdet tm2 @?= Just (-2),
        testCase "mdet tm3 ->> Just 0" $
            mdet tm3 @?= Just 0,
        testCase "mdet dm4x4 ->> Just (-376)" $
            mdet dm4x4 @?= Just (-376),
        testCase "mdet dm5x5 ->> Just 774" $
            mdet dm5x5 @?= Just 774,
        testCase "mdet tm4 ->> Nothing" $
            mdet tm4 @?= Nothing,
        testCase "mdet tm5 ->> Nothing" $
            mdet tm5 @?= Nothing,
        testCase "mdet ungueltig1 ->> Nothing" $
            mdet ungueltig1 @?= Nothing,
        testCase "mdet ungueltig2 ->> Nothing" $
            mdet ungueltig2 @?= Nothing,
        testCase "mdet fm1 ->> Just (-2)" $
            mdet fm1 @?= Just (-2),
        testCase "mdet fm17 ->> Just 7" $
            mdet fm17 @?= Just 7,
        testCase "mdet fm5 ->> Nothing" $
            mdet fm5 @?= Nothing,
        testCase "mdet fm6 ->> Nothing" $
            mdet fm6 @?= Nothing,
        testCase "mdet fm7 ->> Nothing" $
            mdet fm7 @?= Nothing,
        testCase "mdet fm8 ->> Nothing" $
            mdet fm8 @?= Nothing,
        testCase "mdet fm9 ->> Nothing" $
            mdet fm9 @?= Nothing,
        testCase "mdet fm10 ->> Nothing" $
            mdet fm10 @?= Nothing,
        testCase "mdet fm12 ->> Nothing" $
            mdet fm12 @?= Nothing,
        testCase "mdet fm18 ->> Just 0" $
            mdet fm18 @?= Just 0,
        testCase "mdet fm19 ->> Just 0" $
            mdet fm19 @?= Just 0,
        testCase "mdet fm20 ->> Just 0" $
            mdet fm20 @?= Just 0,
        testCase "mfehler :: Matrix ->> ()" $
            show (mfehler :: Matrix) @?= "()",
        testCase "mfehler :: MatrixF ->> ()" $
            show (mfehler :: MatrixF) @?= "()"
    ]
    
a_3 :: TestTree
a_3 =
  testGroup
    "a_3"
    [
        testCase "konv1 ungueltig1 ->> ()" $
            show (konv1 ungueltig1) @?= "()",
        testCase "konv1 ungueltig2 ->> ()" $
            show (konv1 ungueltig2) @?= "()",
        testCase "show (konv1 tm1) ->> show tm1" $
            show (konv1 tm1) @?= show tm1,
        testCase "show (konv1 tm2) ->> show tm2" $
            show (konv1 tm2) @?= show tm2,
        testCase "show (konv1 tm3) ->> show tm3" $
            show (konv1 tm3) @?= show tm3,
        testCase "show (konv1 tm4) ->> show tm4" $
            show (konv1 tm4) @?= show tm4,
        testCase "show (konv1 tm5) ->> show tm5" $
            show (konv1 tm1) @?= show tm1,
        testCase "show (konv1 dm4x4) ->> show dm4x4" $
            show (konv1 dm4x4) @?= show dm4x4,
        testCase "show (konv1 dm5x5) ->> show dm5x5" $
            show (konv1 dm5x5) @?= show dm5x5,
        testCase "konv2 fm7 ->> ()" $
            show (konv2 fm7) @?= "()",
        testCase "konv2 fm8 ->> ()" $
            show (konv2 fm8) @?= "()",
        testCase "konv2 fm9 ->> ()" $
            show (konv2 fm9) @?= "()",
        testCase "konv2 fm10 ->> ()" $
            show (konv2 fm10) @?= "()",
        testCase "konv2 fm1 ->> fm1_konv" $
            konv2 fm1 @?= fm1_konv,
        testCase "konv2 fm2 ->> fm1_konv" $
            konv2 fm2 @?= fm1_konv,
        testCase "konv2 fm3 ->> fm1_konv" $
            konv2 fm3 @?= fm1_konv,
        testCase "konv2 fm4 ->> fm1_konv" $
            konv2 fm4 @?= fm1_konv,
        testCase "konv2 fm5 ->> fm5_konv" $
            konv2 fm5 @?= fm5_konv,
        testCase "konv2 fm6 ->> fm5_konv" $
            konv2 fm6 @?= fm5_konv,
        testCase "konv2 fm17 ->> fm17_konv" $
            konv2 fm17 @?= fm17_konv,
        testCase "konv2 fm18 ->> fm18_konv" $
            konv2 fm18 @?= fm18_konv,
        testCase "konv2 fm19 ->> fm19_konv" $
            konv2 fm19 @?= fm19_konv,
        testCase "konv2 fm20 ->> fm20_konv" $
            konv2 fm20 @?= fm20_konv
    ]

a_6 :: TestTree
a_6 = 
  testGroup
    "a_6"
    [
      testCase "konservenrechner fp2 \"A\" \"A\" ->> Just 0" $
        konservenrechner fp2 "A" "A" @?= Just 0,
      testCase "konservenrechner fp2 \"A\" \"B\" ->> Just 0" $
        konservenrechner fp2 "A" "B" @?= Just 0,
      testCase "konservenrechner fp2 \"B\" \"A\" ->> Nothing" $
        konservenrechner fp2 "B" "A" @?= Nothing,
      testCase "konservenrechner fp3 \"A\" \"C\" ->> Just 1" $
        konservenrechner fp3 "A" "C" @?= Just 1,
      testCase "konservenrechner fp4 \"A\" \"C\" ->> Just 0" $
        konservenrechner fp4 "A" "C" @?= Just 0,
      testCase "konservenrechner fp4 \"B\" \"A\" ->> Just 0" $
        konservenrechner fp4 "B" "A" @?= Just 0,
      testCase "konservenrechner fp5 \"A\" \"G\" ->> Just 1" $
        konservenrechner fp5 "A" "G" @?= Just 1,
      testCase "konservenrechner fp5 \"B\" \"G\" ->> Just 0" $
        konservenrechner fp5 "B" "G" @?= Just 0,
      testCase "konservenrechner fp5 \"I\" \"G\" ->> Nothing" $
        konservenrechner fp5 "I" "G" @?= Nothing,
      testCase "konservenrechner fp5 \"A\" \"H\" ->> Just 2" $
        konservenrechner fp5 "A" "H" @?= Just 2,
      testCase "konservenrechner fp1 \"A\" \"C\" ->> Just 1" $
        konservenrechner fp1 "A" "C" @?= Just 1
    ]
    
