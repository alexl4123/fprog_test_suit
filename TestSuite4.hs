module TestSuite4 where

import Test.Tasty
import Test.Tasty.HUnit

import Control.Exception
import Test.Tasty.HUnit

import Angabe4


spec :: TestTree
spec = testGroup "Exercise 4" [a_1, a_2, a_3, a_4]

-- Test Data Input:

hans = GP "Hans Joerg" (D XXI Feb 1998)
hans2 = GP "Hans Joerg" (D XI Feb 1998)
ulfride = GP "Ulfride" (D I Dez 2001)
kevin = GP "Kevin" (D XXXI Nov 1970)
gertrud = GP "Gertrud" (D XXIX Feb 2000)
karl = GP "Karl" (D XXIX Feb 2001)
maxmustermann = GP "Max Mustermann" (D XXX Feb 2020)
maxmustermann2 = GP "Max Mustermann" (D XXXI Feb 2020)

zahlung_0 = Zahlung (C 100) ZehnProzent (D XXI Feb 1999)
zahlung_1 = Zahlung (C 1000) DreiProzent (D XXXI Dez 1999)
zahlung_2 = Zahlung (C 10000) FuenfProzent (D XXXI Nov 1999)
zahlung_3 = Zahlung (C 500) KeinSkonto (D XXIX Feb 1900)
zahlung_4 = Zahlung (C 515) DreiProzent (D XXX Feb 2000)
zahlung_5 = Zahlung (C 1000) ZehnProzent (D XIV Aug 2020)
zahlung_6 = Zahlung (C 0) KeinSkonto (D XXII Jun 2020)
zahlung_7 = Zahlung (C 1100) KeinSkonto (D XXII Jun 2020)

gutschrift_0 = Gutschrift (C 1000) (D XV Jun 1999)
gutschrift_1 = Gutschrift (C 1) (D XXIX Feb 2001)
gutschrift_2 = Gutschrift (C 50) (D XXXI Sep 2002)
gutschrift_3 = Gutschrift (C 900) (D II Apr 2030)
gutschrift_4 = Gutschrift (C 0) (D III Apr 2030)

-- Test Data Output:

-- waup

a_hans = GP "Hans Joerg" (D XXI Feb 1998) :: P_Geschaeftspartner 
a_hans2 = GP "Hans Joerg" (D XI Feb 1998) :: P_Geschaeftspartner 
a_ulfride = GP "Ulfride" (D I Dez 2001) :: P_Geschaeftspartner
a_kevin = GP "Kevin" (D I Dez 1970) :: P_Geschaeftspartner
a_gertrud = GP "Gertrud" (D XXIX Feb 2000) :: P_Geschaeftspartner
a_karl = GP "Karl" (D I Mar 2001) :: P_Geschaeftspartner

a_zahlung_0 = AP_Zahlung (C 90) (D XXI Feb 1999)
a_zahlung_1 = AP_Zahlung (C 970) (D XXXI Dez 1999)
a_zahlung_2 = AP_Zahlung (C 9500) (D I Dez 1999)
a_zahlung_3 = AP_Zahlung (C 500) (D I Mar 1900)
a_zahlung_4 = AP_Zahlung (C 500) (D I Mar 2000)
a_zahlung_5 = AP_Zahlung (C 900) (D XIV Aug 2020)
a_zahlung_6 = AP_Zahlung (C 0) (D XXII Jun 2020)
a_zahlung_7 = AP_Zahlung (C 1100) (D XXII Jun 2020)

a_gutschrift_0 = P_Gutschrift (C 1000) (D XV Jun 1999)
a_gutschrift_1 = P_Gutschrift (C 1) (D I Mar 2001)
a_gutschrift_2 = P_Gutschrift (C 50) (D I Okt 2002)
a_gutschrift_4 = P_Gutschrift (C 0) (D III Apr 2030)

-- konsolodiere

k_zahlung_0 = K_Zahlung (EC 0 90) (D XXI Feb 1999)
k_zahlung_1 = K_Zahlung (EC 9 70) (D XXXI Dez 1999)
k_zahlung_2 = K_Zahlung (EC 95 9) (D I Dez 1999)
k_zahlung_3 = K_Zahlung (EC 5 0) (D I Mar 1900)
k_zahlung_4 = K_Zahlung (EC 5 0) (D I Mar 2000)
k_zahlung_5 = K_Zahlung (EC 9 0) (D XIV Aug 2020)
k_zahlung_6 = K_Zahlung (EC 0 0) (D XXII Jun 2020)
k_zahlung_7 = K_Zahlung (EC 11 0) (D XXII Jun 2020)

k_gutschrift_0 = K_Gutschrift (EC 10 0) (D XV Jun 1999)
k_gutschrift_1 = K_Gutschrift (EC 0 1) (D I Mar 2001)
k_gutschrift_2 = K_Gutschrift (EC 0 50) (D I Okt 2002)
k_gutschrift_3 = K_Gutschrift (EC 9 0) (D II Apr 2030)
k_gutschrift_4 = K_Gutschrift (EC 0 0) (D III Apr 2030)

-- Tests
a_1 :: TestTree
a_1 =
  testGroup
    "a_1 - waup test"
    [ testCase "0 - waup (hans, zahlung_0) ->> (a_hans, a_zahlung_0)" $
        waup (hans, zahlung_0) @?= (a_hans, a_zahlung_0),
      testCase "1 - waup (ulfride, zahlung_1) ->> (a_ulfride, a_zahlung_1)" $
        waup (ulfride, zahlung_1) @?= (a_ulfride, a_zahlung_1),
      testCase "2 - waup (kevin, zahlung_2) ->> (a_kevin, a_zahlung_2)" $
        waup (kevin, zahlung_2) @?= (a_kevin, a_zahlung_2),
      testCase "3 - waup (gertrud, zahlung_3) ->> (a_gertrud, a_zahlung_3)" $
        waup (gertrud, zahlung_3) @?= (a_gertrud, a_zahlung_3),
      testCase "4 - waup (karl, zahlung_4) ->> (a_karl, a_zahlung_4)" $
        waup (karl, zahlung_4) @?= (a_karl, a_zahlung_4),
      testCase "5 - waup (hans, gutschrift_0) ->> (a_hans, a_gutschrift_0)" $
        waup (hans, gutschrift_0) @?= (a_hans, a_gutschrift_0),
      testCase "6 - waup (hans, gutschrift_1) ->> (a_hans, a_gutschrift_1)" $
        waup (hans, gutschrift_1) @?= (a_hans, a_gutschrift_1),
      testCase "7 - waup (hans, gutschrift_2) ->> (a_hans, a_gutschrift_2)" $
        waup (hans, gutschrift_2) @?= (a_hans, a_gutschrift_2)
    ]

a_2 :: TestTree
a_2 = 
  testGroup
    "a_2 - konsolidiere test"
    [ testCase "0 - konsolodiere [] ->> []"$
        konsolidiere (KB []) @?= KKB [],
      testCase "1 - konsolodiere [(hans,zahlung_0)] ->> [(a_hans,k_zahlung_0)]" $
        konsolidiere (KB [(hans,zahlung_0)]) @?= KKB [(a_hans,k_zahlung_0)],
      testCase "2 - konsolodiere [(hans,zahlung_0),(ulfride,zahlung_1)] ->> [(a_hans,k_zahlung_0),(a_ulfride,k_zahlung_1]" $
        konsolidiere (KB [(hans,zahlung_0),(ulfride,zahlung_1)]) @?= KKB [(a_hans,k_zahlung_0),(a_ulfride,k_zahlung_1)],
       testCase "3 - konsolodiere [(hans,zahlung_0),(ulfride,zahlung_1),(karl,gutschrift_0)] ->> [(a_hans,k_zahlung_0),(a_ulfride,k_zahlung_1),(a_karl,k_gutschrift_0)]" $
        konsolidiere (KB [(hans,zahlung_0),(ulfride,zahlung_1),(karl,gutschrift_0)]) @?= KKB [(a_hans,k_zahlung_0),(a_ulfride,k_zahlung_1),(a_karl,k_gutschrift_0)]
    ]

a_3 :: TestTree
a_3 = 
  testGroup
    "a_3 - saldo test"
    [ testCase "0 - saldo hans [] ->> Keine_Geschaeftsbeziehung" $
        saldo a_hans (KKB []) @?= Keine_Geschaeftsbeziehung,
      testCase "1 - saldo hans [(a_ulfride, k_zahlung_1)] ->> Keine_Geschaeftsbeziehung" $
        saldo a_hans (KKB [(a_ulfride,k_zahlung_1)]) @?= Keine_Geschaeftsbeziehung,
      testCase "2 - saldo hans [(a_hans, k_zahlung_0)] ->> Zahlungssaldo (EC 9 70)" $
        saldo a_hans (KKB [(a_hans, k_zahlung_0)]) @?= Zahlungssaldo (EC 0 90),
      testCase "3 - saldo hans [(a_hans, k_gutschrift_0)] ->> Forderungssaldo (EC 10 0)" $
        saldo a_hans (KKB [(a_hans, k_gutschrift_0)]) @?= Forderungssaldo (EC 10 0),
      testCase "4 - saldo hans [(a_hans, k_gutschrift_0), (a_hans, k_zahlung_0), (a_hans, k_zahlung_1), (a_kevin, k_gutschrift_1), (a_hans2, k_gutschrift_0)] ->> Zahlungssaldo (EC 0 60)" $
        saldo a_hans (KKB [(a_hans, k_gutschrift_0), (a_hans, k_zahlung_0), (a_hans, k_zahlung_1), (a_kevin, k_gutschrift_1), (a_hans2, k_gutschrift_0)]) @?= Zahlungssaldo (EC 0 60),
      testCase "5 - saldo hans [(a_hans, k_zahlung_5), (a_hans, k_gutschrift_3)] ->> Ausgeglichen" $
        saldo a_hans (KKB [(a_hans, k_zahlung_5), (a_hans, k_gutschrift_3)]) @?= Ausgeglichen,
      testCase "6 - saldo hans [(a_hans, k_zahlung_6)] ->> Ausgeglichen" $
        saldo a_hans (KKB [(a_hans, k_zahlung_6)]) @?= Ausgeglichen,
      testCase "7 - saldo hans [(a_hans, k_zahlung_6)] ->> Ausgeglichen" $
        saldo a_hans (KKB [(a_hans, k_zahlung_6)]) @?= Ausgeglichen, 
      testCase "8 - saldo hans [(a_hans, k_gutschrift_4)] ->> Ausgeglichen" $
        saldo a_hans (KKB [(a_hans, k_gutschrift_4)]) @?= Ausgeglichen, 
      testCase "9 - saldo a_hans (KKB [(a_hans, k_gutschrift_0), (a_hans, k_gutschrift_2), (a_hans, k_gutschrift_2), (ulfride, k_gutschrift_0), (a_hans, k_zahlung_7)]) ->> Ausgeglichen" $
        saldo a_hans (KKB [(a_hans, k_gutschrift_0), (a_hans, k_gutschrift_2), (a_hans, k_gutschrift_2), (ulfride, k_gutschrift_0), (a_hans, k_zahlung_7)]) @?= Ausgeglichen 
    ]


a_4 :: TestTree
a_4 = 
  testGroup
    "a_4 - saldieren test"
    [ testCase "0 - saldiere [] ->> []" $
        saldiere (KB []) @?= (SKB []),
      testCase "1 - saldiere [(hans, zahlung_0)] ->> [(hans, Zahlungssaldo (EC 0 90))] " $
        saldiere (KB [(hans,zahlung_0)]) @?= (SKB [(hans,Zahlungssaldo (EC 0 90))]),
      testCase "2 - saldiere [(hans, zahlung_0), (hans, gutschrift_0), (hans, gutschrift_1)] ->> [(hans, Forderungssaldo (EC 9 11))]" $
        saldiere (KB [(hans, zahlung_0), (hans, gutschrift_0), (hans, gutschrift_1)]) @?= (SKB [(hans, Forderungssaldo (EC 9 11))]),
      testCase "3 - saldiere [(ulfride, zahlung_4), (hans, zahlung_0), (kevin, gutschrift_1), (hans2, zahlung_2), (hans, gutschrift_0), (hans, gutschrift_1), (ulfride, zahlung_3)] ->> [(hans, Forderungssaldo (EC 9 11)), (hans2, Zahlungssaldo (EC 95 0)), (kevin, Forderungssaldo (EC 10 0)), (ulfride, Zahlungssaldo (EC 5 0))]" $
        saldiere (KB [(ulfride, zahlung_4), (hans, zahlung_0), (kevin, gutschrift_0), (hans2, zahlung_2), (hans, gutschrift_0), (hans, gutschrift_1), (ulfride, zahlung_3)]) @?= (SKB [(hans2, Zahlungssaldo (EC 95 0)), (hans, Forderungssaldo (EC 9 11)), (kevin, Forderungssaldo (EC 10 0)), (ulfride, Zahlungssaldo (EC 10 0))]),
      testCase "4 - saldiere [(maxmustermann, zahlung_5)] ->> [(maxmustermann, Zahlungssaldo (EC 9 0)]" $
        saldiere (KB [(maxmustermann, zahlung_5)]) @?= (SKB [(maxmustermann, Zahlungssaldo (EC 9 0))]),
	  testCase "5 - saldiere [(maxmustermann, gutschrift_3)] ->> [(maxmustermann, Zahlungssaldo (EC 9 0)]" $
        saldiere (KB [(maxmustermann, gutschrift_3)]) @?= (SKB [(maxmustermann, Forderungssaldo (EC 9 0))]),
	  testCase "6 - saldiere [(maxmustermann, zahlung_5), (maxmustermann, gutschrift_3)] ->> [(maxmustermann, Ausgeglichen)]" $
        saldiere (KB [(maxmustermann, zahlung_5),(maxmustermann, gutschrift_3)]) @?= (SKB [(maxmustermann, Ausgeglichen)]),
	  testCase "7 - saldiere [(maxmustermann2, zahlung_5), (maxmustermann2, gutschrift_3), (maxmustermann, zahlung_5), (maxmustermann, gutschrift_3)] ->> [(maxmustermann, Ausgeglichen), (maxmustermann2, Ausgeglichen)]" $
        saldiere (KB [(maxmustermann2, zahlung_5), (maxmustermann2, gutschrift_3), (maxmustermann, zahlung_5),(maxmustermann, gutschrift_3)]) @?= (SKB [(maxmustermann, Ausgeglichen), (maxmustermann2, Ausgeglichen)]),
      testCase "8 (sollte momentan fehlschlagen) - saldiere [(maxmustermann2, zahlung_5), (maxmustermann2, gutschrift_3), (maxmustermann, zahlung_5), (maxmustermann, gutschrift_3), (maxmustermann2, zahlung_0)] ->> [(maxmustermann, Ausgeglichen), (maxmustermann2, Ausgeglichen)]" $
        saldiere (KB [(maxmustermann2, zahlung_5), (maxmustermann2, gutschrift_3), (maxmustermann, zahlung_5),(maxmustermann, gutschrift_3), (maxmustermann2, zahlung_0)]) @?= (SKB [(maxmustermann, Ausgeglichen), (maxmustermann2, Zahlungssaldo (EC 0 90))])
	]
