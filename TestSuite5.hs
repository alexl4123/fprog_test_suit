module TestSuite5 where

import Test.Tasty
import Test.Tasty.HUnit

import Control.Exception
import Test.Tasty.HUnit

import Angabe5

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

spec :: TestTree
spec = testGroup "Exercise 5" [a_1, a_2, a_3, a_4, a_5]

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
      testCase "show f1 ->> {(N,II),(I,III),(II,IV),(III,V),(IV,VI),(V,VII),(VI,VIII),(VII,IX),(VIII,X),(IX,F),(X,F),(F,F)}" $
        show f1 @?= "{(N,II),(I,III),(II,IV),(III,V),(IV,VI),(V,VII),(VI,VIII),(VII,IX),(VIII,X),(IX,F),(X,F),(F,F)}"
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
      testCase "vereinige int_m1 int_m6 ->> error Fehler" $
        expectError (vereinige int_m1 int_m6 ) ("Fehler"),
      testCase "vereinige int_m2 int_m3 ->> int_m2" $
        sind_gleich (vereinige int_m2 int_m3) int_m2 @?= True,
      testCase "vereinige int_m4 int_m3 ->> int_m7" $
        sind_gleich (vereinige int_m4 int_m3) int_m7 @?= True,
      testCase "schneide int_m1 int_m7 ->> int_m1" $
        sind_gleich (schneide int_m1 int_m7) int_m1 @?= True,
      testCase "schneide int_m2 int_m4 ->> int_m5" $
        sind_gleich (schneide int_m2 int_m4) int_m5 @?= True,
      testCase "schneide int_m4 int_m6 ->> error Fehler" $
        expectError (schneide int_m4 int_m6) ("Fehler"),
      testCase "ziehe_ab int_m4 int_m5 ->> int_m9" $
        sind_gleich (ziehe_ab int_m4 int_m5) int_m9 @?= True,
      testCase "ziehe_ab int_m5 int_m4 ->> int_m1" $
        sind_gleich (ziehe_ab int_m5 int_m4) int_m1 @?= True,
      testCase "ist_teilmenge int_m5 int_m4 ->> True" $
        ist_teilmenge int_m5 int_m4  @?= True,
      
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
        ET f1 == ET f3 @?= True
    ]
