module TestSuite5 where

import Test.Tasty
import Test.Tasty.HUnit

import Control.Exception
import Test.Tasty.HUnit

import Angabe5

-- Hier Test Gruppen hinzufuegen
-- Add test groups here
spec :: TestTree
spec = testGroup "Exercise 5" [a_1, a_2, a_3, a_4, a_5, a_6, a_7, a_8]

-- Das hier nach der `module ... where` Anweisung
-- Das hier irgendwo im code.
expectError :: a -> String -> Assertion
expectError val msg = do
  res <- try (evaluate val)
  case res of
    Left (ErrorCall err) -> err @?= msg
    Right _ -> assertFailure "Expected error call"

b1 = Knoten (Knoten (Blatt 'a') 'b' (Blatt 'c')) 'd' (Blatt 'e')
b2 = Blatt 'a'
b3 = Knoten (Blatt 'x') 'y' (Blatt 'z')

f1 = Fkt (\a -> a + II)
f2 = Fkt (\a -> a + IV)
f3 = Fkt (\a -> II + a)
f4 = Fkt (\a -> a - 3)
f5 = Fkt (\a -> a * 2)
f6 = Fkt (\a -> a + 11)
f7 = Fkt (\a -> a - 15)

int_m1 = leer :: [] Int
int_m2 = [1,2,3,4,5] :: [] Int
int_m3 = [1,2,3,4] :: [] Int
int_m4 = [4,5,6,7] :: [] Int
int_m5 = [4,5] :: [] Int
int_m6 = [1,2,3,2,3] :: [] Int
int_m7 = [1,2,3,4,5,6,7] :: [] Int
int_m8 = [2,4,3,1] :: [] Int
int_m9 = [6,7] :: [] Int

z_m1 = leer :: [] Zahlraum_0_10
z_m2 = [N,I,II,III,IV,V,VI,VII,VIII,IX,X,F] :: [] Zahlraum_0_10
z_m3 = [I,II,III] :: [] Zahlraum_0_10
z_m4 = [N,IV,V,VI,VII,VIII,IX,X,F] :: [] Zahlraum_0_10

f_m1 = leer :: [] Funktion
f_m2 = [f1] :: [] Funktion
f_m3 = [f3] :: [] Funktion
f_m4 = [f2] :: [] Funktion
f_m5 = [f1, f2] :: [] Funktion
f_m6 = [f2, f3] :: [] Funktion

p_m1 = leer :: [] (Paar Int Int)
p_m2 = [P (1,2), P (1,3)] :: [] (Paar Int Int)
p_m3 = [P (1,2)] :: [] (Paar Int Int)
p_m4 = [P (1,3)] :: [] (Paar Int Int)

b_m1 = leer :: [] (Baum Char)
b_m2 = [b2] :: [] (Baum Char)
b_m3 = [b1, b2] :: [] (Baum Char)
b_m4 = [b3, b2] :: [] (Baum Char)
b_m5 = [b1,b2,b3] :: [] (Baum Char)
b_m6 = [b1,b2,b1] :: [] (Baum Char)
b_m7 = [b1] :: [] (Baum Char)

mm1 = leer :: [] (ElemTyp Char)
mm2 = [ET 'a', ET 'b', ET 'c', ET 'd', ET 'e', ET 'a']
mm3 = [ET 'a', ET 'b', ET 'c']
mm4 = [ET 'a', ET 'd', ET 'e']
mm5 = [ET 'b', ET 'd', ET 'a', ET 'a', ET 'c', ET 'e']

phm1 = leer :: [] (PH_ElemTyp Char Int (Baum Char) (Paar Int Char) Funktion)
phm2 = [A 'B', B 2, C b2, D (P (1,'A')), E f1]
phm3 = [E f1, E f1, A 'C']
phm4 = [A 'B', B 2, C b2]
phm5 = [D (P (1, 'A')), E f1]
phm6 = [E f1, C b2, B 2, D (P (1, 'A')), A 'B']

phmm1 = leer :: [] (PH_ElemTyp' Char Int String)
phmm2 = [Q 'A', Q 'A', Q 'A', Q 'B', R 5] :: [] (PH_ElemTyp' Char Int String)
phmm3 = [Q 'A', Q 'A', R 5] :: [] (PH_ElemTyp' Char Int String)
phmm4 = [Q 'A', Q 'B'] :: [] (PH_ElemTyp' Char Int String)
phmm5 = [Q 'A', R 5, Q 'A', Q 'B', Q 'A'] :: [] (PH_ElemTyp' Char Int String)
phmm6 = [Q 'A'] :: [] (PH_ElemTyp' Char Int String)

-------------------------------------------------------------------------
-- TESTS
-------------------------------------------------------------------------
a_1 :: TestTree
a_1 =
  testGroup
    "a_1"
    [ testCase "I + I ->> II" $
        I + I @?= II,
      testCase "F + V ->> F" $
        F + V @?= F,
      testCase "III + F ->> F" $
        III + F @?= F,
      testCase "V - III ->> II" $
        V - III @?= II,
      testCase "V - VI ->> F" $
        V - VI @?= F,
      testCase "III * II ->> VI" $
        III * II @?= VI,
      testCase "V * III ->> F" $
        V * III @?= F,
      testCase "abs III -> III" $
        abs III @?= III,
      testCase "signum N -> 0" $
        signum N @?= 0,
      testCase "signum III -> 1" $
        signum III @?= 1,
      testCase "fromInteger 7 -> VII" $
        fromInteger 7 @?= VII,
      testCase "fromInteger 11 -> F" $
        fromInteger 11 @?= F
    ]

a_2 :: TestTree
a_2 = 
  testGroup
    "a_2"
    [ testCase "f1 == f2 ->> False" $
        (f1 == f2) @?= False,
      testCase "f1 == f3 ->> True" $
        (f1 == f3) @?= True,
      testCase "f6 == f7 ->> True" $
        (f6 == f7) @?= True,
      testCase "f4 == f5 ->> False" $
        (f4 == f5) @?= False,
      testCase "show f1 ->> {(N,II),(I,III),(II,IV),(III,V),(IV,VI),(V,VII),(VI,VIII),(VII,IX),(VIII,X),(IX,F),(X,F),(F,F)}" $
        show f1 @?= "{(N,II),(I,III),(II,IV),(III,V),(IV,VI),(V,VII),(VI,VIII),(VII,IX),(VIII,X),(IX,F),(X,F),(F,F)}",
      testCase "show f2 ->> {(N,IV),(I,V),(II,VI),(III,VII),(IV,VIII),(V,IX),(VI,X),(VII,F),(VIII,F),(IX,F),(X,F),(F,F)}" $
        show f2 @?= "{(N,IV),(I,V),(II,VI),(III,VII),(IV,VIII),(V,IX),(VI,X),(VII,F),(VIII,F),(IX,F),(X,F),(F,F)}",
      testCase "show f3 ->> {(N,II),(I,III),(II,IV),(III,V),(IV,VI),(V,VII),(VI,VIII),(VII,IX),(VIII,X),(IX,F),(X,F),(F,F)}" $
        show f3 @?= "{(N,II),(I,III),(II,IV),(III,V),(IV,VI),(V,VII),(VI,VIII),(VII,IX),(VIII,X),(IX,F),(X,F),(F,F)}",
      testCase "show f4 ->> {(N,F),(I,F),(II,F),(III,N),(IV,I),(V,II),(VI,III),(VII,IV),(VIII,V),(IX,VI),(X,VII),(F,F)}" $
        show f4 @?= "{(N,F),(I,F),(II,F),(III,N),(IV,I),(V,II),(VI,III),(VII,IV),(VIII,V),(IX,VI),(X,VII),(F,F)}",
      testCase "show f5 ->> {(N,N),(I,II),(II,IV),(III,VI),(IV,VIII),(V,X),(VI,F),(VII,F),(VIII,F),(IX,F),(X,F),(F,F)}" $
        show f5 @?= "{(N,N),(I,II),(II,IV),(III,VI),(IV,VIII),(V,X),(VI,F),(VII,F),(VIII,F),(IX,F),(X,F),(F,F)}",
      testCase "show f6 ->> {(N,F),(I,F),(II,F),(III,F),(IV,F),(V,F),(VI,F),(VII,F),(VIII,F),(IX,F),(X,F),(F,F)}" $
        show f6 @?= "{(N,F),(I,F),(II,F),(III,F),(IV,F),(V,F),(VI,F),(VII,F),(VIII,F),(IX,F),(X,F),(F,F)}",
      testCase "show f7 ->> {(N,F),(I,F),(II,F),(III,F),(IV,F),(V,F),(VI,F),(VII,F),(VIII,F),(IX,F),(X,F),(F,F)}" $
        show f7 @?= "{(N,F),(I,F),(II,F),(III,F),(IV,F),(V,F),(VI,F),(VII,F),(VIII,F),(IX,F),(X,F),(F,F)}"
	]

a_3 :: TestTree
a_3 = 
  testGroup
    "a_3"
    [ testCase "sind_gleich int_m1 int_m2 ->> False" $
        sind_gleich int_m1 int_m2 @?= False,
      testCase "sind_gleich int_m8 int_m3 ->> True" $
        sind_gleich int_m8 int_m3 @?= True,
      testCase "sind_gleich int_m8 int_m6 ->> error Fehler" $
        expectError (sind_gleich int_m8 int_m6) ("Fehler"),
      testCase "vereinige int_m1 int_m2 ->> int_m2" $
        sind_gleich (vereinige int_m1 int_m2) int_m2 @?= True,
      testCase "vereinige int_m2 int_m1 ->> int_m2" $
        sind_gleich (vereinige int_m2 int_m1) int_m2 @?= True,
      testCase "vereinige int_m1 int_m6 ->> error Fehler" $
        expectError (vereinige int_m1 int_m6) ("Fehler"),
      testCase "vereinige int_m6 int_m1 ->> error Fehler" $
        expectError (vereinige int_m6 int_m1) ("Fehler"),
      testCase "vereinige int_m2 int_m3 ->> int_m2" $
        sind_gleich (vereinige int_m2 int_m3) int_m2 @?= True,
      testCase "vereinige int_m4 int_m3 ->> int_m7" $
        sind_gleich (vereinige int_m4 int_m3) int_m7 @?= True,
      testCase "vereinige int_m4 int_m4 ->> int_m4" $
        sind_gleich (vereinige int_m4 int_m4) int_m4 @?= True,
      testCase "schneide int_m1 int_m7 ->> int_m1" $
        sind_gleich (schneide int_m1 int_m7) int_m1 @?= True,
      testCase "schneide int_m2 int_m4 ->> int_m5" $
        sind_gleich (schneide int_m2 int_m4) int_m5 @?= True,
      testCase "schneide int_m5 int_m9 ->> int_m1" $
        sind_gleich (schneide int_m5 int_m9) int_m1 @?= True,
      testCase "schneide int_m8 int_m8 ->> int_m8" $
        sind_gleich (schneide int_m8 int_m8) int_m8 @?= True,
      testCase "schneide int_m4 int_m6 ->> error Fehler" $
        expectError (schneide int_m4 int_m6) ("Fehler"),
      testCase "ziehe_ab int_m4 int_m5 ->> int_m9" $
        sind_gleich (ziehe_ab int_m4 int_m5) int_m9 @?= True,
      testCase "ziehe_ab int_m5 int_m4 ->> int_m1" $
        sind_gleich (ziehe_ab int_m5 int_m4) int_m1 @?= True,
      testCase "ziehe_ab int_m5 int_m9 ->> int_m5" $
        sind_gleich (ziehe_ab int_m5 int_m9) int_m5 @?= True,
      testCase "ziehe_ab int_m5 int_m5 ->> int_m1" $
        sind_gleich (ziehe_ab int_m5 int_m5) int_m1 @?= True,
      testCase "ziehe_ab int_m2 int_m6 ->> error Fehler" $
        expectError (ziehe_ab int_m2 int_m6) ("Fehler"),
      testCase "ziehe_ab int_m6 int_m3 ->> error Fehler" $
        expectError (ziehe_ab int_m6 int_m3) ("Fehler"),
      testCase "ist_teilmenge int_m5 int_m4 ->> True" $
        ist_teilmenge int_m5 int_m4  @?= True,
      testCase "ist_teilmenge int_m5 int_m2 ->> True" $
        ist_teilmenge int_m5 int_m2  @?= True,
      testCase "ist_teilmenge int_m2 int_m5 ->> False" $
        ist_teilmenge int_m2 int_m5  @?= False,
      testCase "ist_teilmenge int_m9 int_m9 ->> True" $
        ist_teilmenge int_m9 int_m9  @?= True,
      testCase "ist_teilmenge int_m1 int_m9 ->> True" $
        ist_teilmenge int_m1 int_m9  @?= True,
      testCase "ist_teilmenge int_m1 int_m6 ->> True" $
        expectError (ist_teilmenge int_m1 int_m6) ("Fehler"),
      testCase "ist_teilmenge int_m6 int_m6 ->> True" $
        expectError (ist_teilmenge int_m6 int_m6) ("Fehler"),
      testCase "anzahl 2 int_m1 ->> 0" $
        anzahl 2 int_m1  @?= 0,
      testCase "anzahl 2 int_m5 ->> 0" $
        anzahl 2 int_m5  @?= 0,
      testCase "anzahl 6 int_m4 ->> 1" $
        anzahl 6 int_m4  @?= 1,
      testCase "anzahl 2 int_m6 ->> error Fehler" $
        expectError (anzahl 2 int_m6) ("Fehler"),
      
      testCase "vereinige z_m1 z_m2 ->> z_m2" $
        sind_gleich (vereinige z_m1 z_m2) z_m2 @?= True,
      testCase "schneide z_m1 z_m2 ->> z_m1" $
        sind_gleich (schneide z_m1 z_m2) z_m1 @?= True,
      testCase "schneide z_m2 z_m3 ->> z_m3" $
        sind_gleich (schneide z_m2 z_m3) z_m3 @?= True,
      testCase "ziehe_ab z_m2 z_m3 ->> z_m4" $
        sind_gleich (ziehe_ab z_m2 z_m3) z_m4 @?= True,
      testCase "ziehe_ab z_m3 z_m2 ->> z_m1" $
        sind_gleich (ziehe_ab z_m3 z_m2) z_m1 @?= True,
 
      testCase "sind_gleich f_m1 f_m2 ->> False" $
        sind_gleich f_m1 f_m2 @?= False,
      testCase "sind_gleich f_m2 f_m3 ->> True" $
        sind_gleich f_m2 f_m3 @?= True,
      testCase "sind_gleich f_m5 f_m6 ->> True" $
        sind_gleich f_m5 f_m6 @?= True,
      testCase "vereinige f_m1 f_m2 ->> f_m2" $
        sind_gleich (vereinige f_m1 f_m2) f_m2 @?= True,
      testCase "vereinige f_m2 f_m3 ->> f_m2" $
        sind_gleich (vereinige f_m2 f_m3) f_m2 @?= True,
      testCase "vereinige f_m2 f_m3 ->> f_m3" $
        sind_gleich (vereinige f_m2 f_m3) f_m3 @?= True,
      testCase "vereinige f_m2 f_m4 ->> f_m5" $
        sind_gleich (vereinige f_m2 f_m3) f_m2 @?= True
    ]

a_4 :: TestTree
a_4 = 
  testGroup
    "a_4"
    [ testCase "sind_gleich p_m1 p_m2 ->> False" $
        sind_gleich p_m1 p_m2 @?= False,
      testCase "sind_gleich p_m1 p_m1 ->> True" $
        sind_gleich p_m1 p_m1 @?= True,
      testCase "vereinige p_m3 p_m4 ->> p_m2" $
        sind_gleich (vereinige p_m3 p_m4) p_m2 @?= True,
      testCase "vereinige p_m2 p_m3 ->> p_m2" $
        sind_gleich (vereinige p_m2 p_m3) p_m2 @?= True,
      testCase "schneide p_m3 p_m4 ->> p_m1" $
        sind_gleich (schneide p_m3 p_m4) p_m1 @?= True,
      testCase "schneide p_m2 p_m3 ->> p_m3" $
        sind_gleich (schneide p_m2 p_m3) p_m3 @?= True,
      testCase "ziehe_ab p_m2 p_m3 ->> p_m4" $ 
        sind_gleich (ziehe_ab p_m2 p_m3) p_m4 @?= True,

      testCase "sind_gleich b_m1 b_m6 ->> error Fehler" $
        expectError (sind_gleich b_m1 b_m6) ("Fehler"),
      testCase "sind_gleich b_m1 b_m2 ->> False" $
        sind_gleich b_m1 b_m2 @?= False,
      testCase "vereinige b_m3 b_m4 ->> p_m5" $
        sind_gleich (vereinige b_m3 b_m4) b_m5 @?= True,
      testCase "schneide b_m3 b_m4 ->> b_m2" $
        sind_gleich (schneide b_m3 b_m4) b_m2 @?= True,
      testCase "ziehe-ab b_m3 b_m4 ->> b_m7" $
        sind_gleich (ziehe_ab b_m3 b_m4) b_m7 @?= True,
      testCase "anzahl b1 b_m2 ->> 0" $
        anzahl b1 b_m2 @?= 0,
      testCase "anzahl b1 b_m3 ->> 1" $ 
        anzahl b1 b_m3 @?= 1,
      testCase "anzahl b1 b_m6 ->> error Fehler" $
        expectError (anzahl b1 b_m6) ("Fehler")
    ]

a_5 :: TestTree
a_5 = 
  testGroup
    "a_5"
    [ testCase "ET 'a' == ET 'b' ->> False" $
        ET 'a' == ET 'b' @?= False,
      testCase "ET f1 == ET f3 ->> True" $
        ET f1 == ET f3 @?= True,
      testCase "show p_m1 ->> []" $
        show p_m1 @?= "[]",
      testCase "show p_m2 ->> [P (1,2),P (1,3)]" $
        show p_m2 @?= "[P (1,2),P (1,3)]",
      testCase "show p_m3 ->> [P (1,2)]" $
        show p_m3 @?= "[P (1,2)]",
      testCase "show p_m4 ->> [P (1,3)]" $
        show p_m4 @?= "[P (1,3)]",
      testCase "show b_m1 ->> []" $
        show b_m1 @?= "[]",
      testCase "show b_m2 ->> [Blatt 'a']" $
        show b_m2 @?= "[Blatt 'a']",
      testCase "show b_m3 ->> [Knoten (Knoten (Blatt 'a') 'b' (Blatt 'c')) 'd' (Blatt 'e'),Blatt 'a']" $
        show b_m3 @?= "[Knoten (Knoten (Blatt 'a') 'b' (Blatt 'c')) 'd' (Blatt 'e'),Blatt 'a']",
      testCase "show b_m4 ->> [Knoten (Blatt 'x') 'y' (Blatt 'z'),Blatt 'a']" $
        show b_m4 @?= "[Knoten (Blatt 'x') 'y' (Blatt 'z'),Blatt 'a']",
      testCase "show b_m5 ->> [Knoten (Knoten (Blatt 'a') 'b' (Blatt 'c')) 'd' (Blatt 'e'),Blatt 'a',Knoten (Blatt 'x') 'y' (Blatt 'z')]" $
        show b_m5 @?= "[Knoten (Knoten (Blatt 'a') 'b' (Blatt 'c')) 'd' (Blatt 'e'),Blatt 'a',Knoten (Blatt 'x') 'y' (Blatt 'z')]",
      testCase "show b_m6 ->> [Knoten (Knoten (Blatt 'a') 'b' (Blatt 'c')) 'd' (Blatt 'e'),Blatt 'a',Knoten (Knoten (Blatt 'a') 'b' (Blatt 'c')) 'd' (Blatt 'e')]" $
        show b_m6 @?= "[Knoten (Knoten (Blatt 'a') 'b' (Blatt 'c')) 'd' (Blatt 'e'),Blatt 'a',Knoten (Knoten (Blatt 'a') 'b' (Blatt 'c')) 'd' (Blatt 'e')]",
      testCase "show b_m7 ->> [Knoten (Knoten (Blatt 'a') 'b' (Blatt 'c')) 'd' (Blatt 'e')]" $
        show b_m7 @?= "[Knoten (Knoten (Blatt 'a') 'b' (Blatt 'c')) 'd' (Blatt 'e')]"
    ]

a_6 :: TestTree
a_6 = 
  testGroup
    "a_6"
    [ testCase "sind_gleich mm1 mm2 ->> False" $
        sind_gleich mm1 mm2 @?= False,
      testCase "sind_gleich mm2 mm5 ->> True" $
        sind_gleich mm2 mm5 @?= True,
      testCase "sind_gleich mm2 mm3 ->> False" $
        sind_gleich mm2 mm3 @?= False,
      testCase "ist_teilmenge mm3 mm2 ->> True" $
        ist_teilmenge mm3 mm2 @?= True,
      testCase "ist_teilmenge mm3 mm2 ->> False" $
        ist_teilmenge mm2 mm3 @?= False,
      testCase "ist_obermenge mm3 mm2 ->> True" $
        ist_obermenge mm2 mm3 @?= True,
      testCase "ist_teilmenge mm2 mm5 ->> True" $
        ist_teilmenge mm2 mm5 @?= True, 
      testCase "vereinige mm1 mm2 ->> mm2" $
        sind_gleich (vereinige mm1 mm2) mm2 @?= True,
      testCase "vereinige mm3 mm4 ->> mm2" $
        sind_gleich (vereinige mm3 mm4) mm2 @?= True,
      testCase "vereinige mm4 mm3 ->> mm2" $
        sind_gleich (vereinige mm4 mm3) mm2 @?= True,
      testCase "schneide mm2 mm5 ->> mm5" $
        sind_gleich (schneide mm2 mm5) mm5 @?= True,
      testCase "schneide mm2 mm3 ->> mm3" $
        sind_gleich (schneide mm2 mm3) mm3 @?= True,
      testCase "schneide mm3 mm2 ->> mm3" $
        sind_gleich (schneide mm3 mm2) mm3 @?= True,
      testCase "ziehe_ab mm2 mm3 ->> mm4" $
        sind_gleich (ziehe_ab mm2 mm3) mm4 @?= True,
      testCase "ziehe_ab mm3 mm2 ->> mm1" $
        sind_gleich (ziehe_ab mm3 mm2) mm1 @?= True,
      testCase "ziehe_ab mm2 mm1 ->> mm2" $
        sind_gleich (ziehe_ab mm2 mm1) mm2 @?= True,
      testCase "anzahl 'a' mm1 ->> 0" $
        anzahl (ET 'a') mm1 @?= 0,
      testCase "anzahl 'a' mm2 ->> 2" $
        anzahl (ET 'a') mm2 @?= 2
    ]

a_7 :: TestTree
a_7 =
  testGroup
    "a_7"
    [ testCase "sind_gleich phm1 phm1 ->> True" $
        sind_gleich phm1 phm1 @?= True,
      testCase "sind_gleich phm1 phm2 ->> False" $
        sind_gleich phm1 phm2 @?= False,
      testCase "sind_gleich phm2 phm6 ->> True" $
        sind_gleich phm2 phm6 @?= True,
      testCase "sind_gleich phm3 phm6 ->> Error Fehler" $
        expectError (sind_gleich phm3 phm6) ("Fehler"),
      testCase "ist_teilmenge phm2 phm6 ->> True" $
        ist_teilmenge phm2 phm6 @?= True,
      testCase "ist_obermenge phm2 phm6 ->> True" $
        ist_obermenge phm2 phm6 @?= True,
      testCase "vereinige phm1 phm2 ->> phm2" $
        sind_gleich (vereinige phm1 phm2) phm2 @?= True,
      testCase "vereinige phm5 phm4 ->> phm2" $
        sind_gleich (vereinige phm5 phm4) phm2 @?= True,
      testCase "vereinige phm6 phm2 ->> phm2" $
        sind_gleich (vereinige phm6 phm2) phm2 @?= True,
      testCase "schneide phm2 phm1 ->> phm1" $
        sind_gleich (schneide phm2 phm1) phm1 @?= True,
      testCase "schneide phm2 phm4 ->> phm4" $
        sind_gleich (schneide phm2 phm4) phm4 @?= True,
      testCase "ziehe_ab phm2 phm6 ->> phm1" $
        sind_gleich (ziehe_ab phm2 phm6) phm1 @?= True,
      testCase "ziehe_ab phm2 phm5 ->> phm4" $
        sind_gleich (ziehe_ab phm2 phm5) phm4 @?= True
    ]

a_8 :: TestTree
a_8 = 
  testGroup
    "a_8"
    [ testCase "sind_gleich phmm1 phmm1 ->> True" $
        sind_gleich phmm1 phmm1 @?= True,
      testCase "sind_gleich phmm2 phmm5 ->> True" $
        sind_gleich phmm2 phmm5 @?= True,
      testCase "sind_gleich phmm3 phmm5 ->> False" $
        sind_gleich phmm3 phmm5 @?= False,
      testCase "ist_teilmenge phmm3 phmm2 ->> True" $
        ist_teilmenge phmm3 phmm2 @?= True,
      testCase "ist_obermenge phmm4 phmm3 ->> False" $
        ist_obermenge phmm4 phmm3 @?= False,
      testCase "vereinige phmm1 phmm2 ->> phmm2" $
        sind_gleich (vereinige phmm1 phmm2) phmm2 @?= True,
      testCase "vereinige phmm3 phmm4 ->> phmm2" $
        sind_gleich (vereinige phmm3 phmm4) phmm2 @?= True,
      testCase "schneide phmm2 phmm3 ->> phmm3" $
        sind_gleich (schneide phmm2 phmm3) phmm3 @?= True,
      testCase "schneide phmm3 phmm4 ->> phmm6" $
        sind_gleich (schneide phmm3 phmm4) phmm6 @?= True,
      testCase "ziehe_ab phmm2 phmm3 ->> phmm4" $
        sind_gleich (ziehe_ab phmm2 phmm3) phmm4 @?= True,
      testCase "ziehe_ab phmm3 phmm2 ->> phmm1" $
        sind_gleich (ziehe_ab phmm3 phmm2) phmm1 @?= True
    ]











