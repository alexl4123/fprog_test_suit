module TestSuite2 where

import Test.Tasty
import Test.Tasty.HUnit

import Angabe2

spec :: TestTree
spec = testGroup "Exercise 2" [a_2, a_3, a_4, a_5]

-- Daten:
-- Gueltige Daten

d0 = ((D XXII Okt 2021)) :: Datum
d1 = ((D XXIX Feb 2020)) :: Datum
d2 = ((D XXXI Jan 2020)) :: Datum
d3 = ((D XXX Apr 2020)) :: Datum
d4 = ((D XXIX Feb 2000)) :: Datum
d5 = ((D XXIII Okt 2021)) :: Datum

d6 = ((D XXVIII Feb 2020)) :: Datum
d7 = ((D XXVIII Feb 2021)) :: Datum
d8 = ((D III Mar 2020)) :: Datum
d9 = ((D III Mar 2021)) :: Datum

-- Ungueltige Daten:
ud0 = ((D XXIX Feb 2021)) :: Datum
ud1 = ((D XXIX Feb 1900)) :: Datum
ud2 = ((D XXXI Feb 2020)) :: Datum
ud3 = ((D XXXI Apr 2020)) :: Datum

-- Kontrollzeitpunkte (KZP's)
-- Gueltige KZP's:
kzp0 = (d0, (U (Schlag,Acht,VM)))
kzp1 = (d1, (U (Schlag,Acht,VM)))
kzp2 = (d5, (U (Viertel,Acht,VM)))
kzp3 = (d5, (U (Halb,Acht,VM)))

kzp4 = (d8, (U (Halb,Acht,VM)))
kzp5 = (d9, (U (Halb,Acht,VM)))

ukzp0 = (ud0, (U (Dreiviertel,Zehn,NM)))

-- 3G Nachweise
dgs0 = Getestet Antigen (D I Mar 2020) (U (Viertel,Acht,VM)) :: DreiG_Status
dgs1 = Getestet PCR (D XX Okt 2021) (U (Viertel,Acht,VM)) :: DreiG_Status

dgs2 = Getestet PCR d6 (U (Halb,Acht,VM)) :: DreiG_Status
dgs3 = Getestet PCR d7 (U (Halb,Acht,VM)) :: DreiG_Status

udgs0 = Getestet Antigen (D XXIX Feb 1999) (U (Viertel,Neun,NM)) :: DreiG_Status

impf0 = Geimpft (AstraZeneca,Zweimal)
impf1 = Geimpft (JundJ,Einmal)

uimpf0 = Geimpft (BioNTec,Einmal)
uimpf1 = Geimpft (Sputnik,Zweimal)

genesen0 = Genesen :: DreiG_Status

udrei0 = Udrei :: DreiG_Status

-- Personen
bgm = P (Vorname "Michael") (Nachname "Ludwig") dgs1 :: Person
genesen = P (Vorname "VN - Genesen") (Nachname "nn - Genesen") genesen0 :: Person
udrei = P (Vorname "vn - udrei") (Nachname "nn - udrei") udrei0 :: Person

get0 = P (Vorname "VN - getestet0") (Nachname "nn - Getestet0") dgs0 :: Person
get1 = P (Vorname "VN - getestet1") (Nachname "nn - Getestet1") dgs2 :: Person
get2 = P (Vorname "VN - getestet2") (Nachname "nn - Getestet2") dgs3 :: Person


uget0 = P (Vorname "VN - getestet0") (Nachname "nn - Getestet0") udgs0 :: Person

pgeimpft0 = P (Vorname "VN - geimpft") (Nachname "nn - geimpft") impf0 :: Person
pgeimpft1 = P (Vorname "VN - geimpft") (Nachname "nn - geimpft") impf1 :: Person

upgeimpft0 = P (Vorname "VN - geimpft") (Nachname "nn - geimpft") uimpf0 :: Person
upgeimpft1 = P (Vorname "VN - geimpft") (Nachname "nn - geimpft") uimpf1 :: Person


{--------------------------------------------------------------
 - Begin Tests
 -------------------------------------------------------------}

datumTest :: TestTree
datumTest = 
  testGroup
    "datumGueltig"
    [ testCase "kzp0" $
        True @=? datumGueltig d0,
      testCase "kzp1" $
        True @=? datumGueltig d1,
      testCase "kzp2" $
        True @=? datumGueltig d2,
      testCase "kzp3" $
        True @=? datumGueltig d3,
      testCase "kzp4" $
        True @=? datumGueltig d4,
      testCase "ukzp0" $
        False @=? datumGueltig ud0,
      testCase "ukzp1" $
        False @=? datumGueltig ud1,
      testCase "ukzp2" $
        False @=? datumGueltig ud2,
      testCase "ukzp3" $
        False @=? datumGueltig ud3
    ]  

a_2 :: TestTree
a_2 =
  testGroup
    "einzulassen"
    [ testCase "0 - einzulassen (bgm,DreiG,kzp1) ->> Einlassen" $
        Einlassen @=? einzulassen (bgm,DreiG,kzp0),
      testCase "1 - einzulassen (genesen,DreiG,kzp1) ->> Einlassen" $
        Einlassen @=? einzulassen (genesen,DreiG,kzp1),
      testCase "2 - einzulassen (udrei,DreiG,kzp1) ->> Abweisen" $
        Abweisen @=? einzulassen (udrei,DreiG,kzp1),
      testCase "3 - einzulassen (get0,DreiG,kzp1) ->> Einlassen" $
        Abweisen @=? einzulassen (get0,DreiG,kzp1),
      testCase "4 - einzulassen (get0,ZweieinhalbG,kzp1) ->> Abweisen" $
        Abweisen @=? einzulassen (get0,ZweieinhalbG,kzp1),
      testCase "5 - einzulassen (uget0,DreiG,kzp1) ->> Ungueltig" $
        Ungueltig @=? einzulassen (uget0,DreiG,kzp1),
      testCase "6 - einzulassen (get0,DreiG,kzp0) ->> Abweisen" $
        Abweisen @=? einzulassen (get0,DreiG,kzp0),
      testCase "7 - einzulassen (get0,ZweieinhalbG,ukzp0) ->> Ungueltig" $
        Ungueltig @=? einzulassen (get0,ZweieinhalbG,ukzp0),
      testCase "8 - einzulassen (bgm,ZweieinhalbG,kzp2) ->> Einlassen" $
        Einlassen @=? einzulassen (bgm,ZweieinhalbG,kzp2),
      testCase "9 - einzulassen (bgm,ZweieinhalbG,kzp3) ->> Abweisen" $
        Abweisen @=? einzulassen (bgm,ZweieinhalbG,kzp3),
      testCase "10 - einzulassen (bgm,ZweiG,kzp2) ->> Abweisen" $
        Abweisen @=? einzulassen (bgm,ZweiG,kzp2),

      testCase "11 - einzulassen (pgeimpft0,ZweiG,kzp2) ->> Einlassen" $
        Einlassen @=? einzulassen (pgeimpft0,ZweiG,kzp2),
      testCase "12 - einzulassen (pgeimpft1,DreiG,kzp2) ->> Einlassen" $
        Einlassen @=? einzulassen (pgeimpft1,DreiG,kzp2),
      testCase "13 - einzulassen (upgeimpft0,ZweieinhalbG,kzp2) ->> Abweisen" $
        Abweisen @=? einzulassen (upgeimpft0,ZweieinhalbG,kzp2),
      testCase "14 - einzulassen (upgeimpft1,ZweiG,kzp2) ->> Abweisen" $
        Abweisen @=? einzulassen (upgeimpft1,ZweiG,kzp2),
      testCase "15 - einzulassen (pgeimpft0,ZweiG,kzp2) ->> Ungueltig" $
        Ungueltig @=? einzulassen (pgeimpft0,ZweiG,ukzp0),
 
      testCase "16 - einzulassen (get1,DreiG,kzp4) ->> Abweisen" $
        Abweisen @=? einzulassen (get1,DreiG,kzp4),
      testCase "17 - einzulassen (get2,DreiG,kzp5) ->> Einlassen" $
        Einlassen @=? einzulassen (get2,DreiG,kzp5)
 
    ]


a_3 :: TestTree
a_3 = 
  testGroup
    "einzulassende"
    [ testCase "0 - einzulassende [bgm,uget0,genesen,udrei,get0] DreiG kzp0 ->> [\"Michael Ludwig\", \"VN - Genesen nn - Genesen\",...]" $
        ["Michael Ludwig","VN - Genesen nn - Genesen"] @=? einzulassende [bgm,uget0,genesen,udrei,get0] DreiG kzp0,
      testCase "1 - einzulassende [bgm,uget0,genesen,udrei,get0] DreiG ukzp0 ->> []" $
        [] @=? einzulassende [bgm,uget0,genesen,udrei,get0] DreiG ukzp0
    ]


a_4 :: TestTree
a_4 = 
  testGroup
    "einzulassende_abzuweisende"
    [ testCase "0 - einzulassende_abzuweisende [bgm,uget0,genesen,udrei,get0] DreiG kzp0 ->> ([[\"Michael Ludwig\", \"VN - Genesen nn - Genesen\"]],[[\"vn - udrei nn - udrei\", \"VN - getestet0 nn - Getestet0\"]])" $
        (["Michael Ludwig","VN - Genesen nn - Genesen"],["vn - udrei nn - udrei", "VN - getestet0 nn - Getestet0"]) @=? einzulassende_abzuweisende [bgm,uget0,genesen,udrei,get0] DreiG kzp0,
      testCase "1 - einzulassende_abzuweisende [bgm,uget0,genesen,udrei,get0] DreiG ukzp0 ->> ([],[])" $
        ([],[]) @=? einzulassende_abzuweisende [bgm,uget0,genesen,udrei,get0] DreiG ukzp0
    ]


a_5 :: TestTree
a_5 = 
  testGroup
    "show"
    [ testCase "U0 - show (U (Viertel,Zwoelf,VM)) ->> \"11:15 Uhr\"" $
        "11:15 Uhr" @=? show (U (Viertel,Zwoelf,VM)),
      testCase "U1 - show (U (Viertel,Zwoelf,NM)) ->> \"23:15 Uhr\"" $
        "23:15 Uhr" @=? show (U (Viertel,Zwoelf,NM)),
      testCase "U2 - show (U (Dreiviertel,Zwoelf,VM)) ->> \"11:45 Uhr\"" $
        "11:45 Uhr" @=? show (U (Dreiviertel,Zwoelf,VM)),

      testCase "U3 - show (U (Dreiviertel,Zwoelf,NM)) ->> \"23:45 Uhr\"" $
        "23:45 Uhr" @=? show (U (Dreiviertel,Zwoelf,NM)),

      testCase "U4 - show (U (Schlag,Zwoelf,VM)) ->> \"12:00 Uhr\"" $
        "12:00 Uhr" @=? show (U (Schlag,Zwoelf,VM)),
      testCase "U5 - show (U (Schlag,Zwoelf,NM)) ->> \"24:00 Uhr\"" $
        "24:00 Uhr" @=? show (U (Schlag,Zwoelf,NM)),

      testCase "U6 - show (U (Halb,Sechs,VM)) ->> \"05:30 Uhr\"" $
        "05:30 Uhr" @=? show (U (Halb,Sechs,VM)),
      testCase "U7 - show (U (Halb,Sechs,NM)) ->> \"17:30 Uhr\"" $
        "17:30 Uhr" @=? show (U (Halb,Sechs,NM)),

      testCase "D0 - show (D XXII Okt 2021) ->> \"22.10.2021\"" $
        "22.10.2021" @=? show (D XXII Okt 2021),
      testCase "D1 - show (D XXIV Dez 2412) ->> \"24.12.2412\"" $
        "24.12.2412" @=? show (D XXIV Dez 2412),

      testCase "D2 - show (D XXXI Feb 1234) ->> \"Datum ungueltig\"" $
        "Datum ungueltig" @=? show (D XXXI Feb 1234),
      testCase "D3 - show (D XXIX Feb 2021) ->> \"Datum ungueltig\"" $
        "Datum ungueltig" @=? show (D XXIX Feb 2021),
      testCase "D4 - show (D XXIX Feb 2020) ->> \"29.2.2020\"" $
        "29.2.2020" @=? show (D XXIX Feb 2020),
      testCase "D5 - show (D XXIX Feb 1900) ->> \"Datum ungueltig\"" $
        "Datum ungueltig" @=? show (D XXIX Feb 1900),
      testCase "D6 - show (D XXIX Feb 2000) ->> \"29.2.2000\"" $
        "29.2.2000" @=? show (D XXIX Feb 2000),

      testCase "D7 - show (D IV Nov 2100) ->> \"4.11.2100\"" $
        "4.11.2100" @=? show (D IV Nov 2100),
      testCase "D8 - show (D XXXI Dez 2100) ->> \"31.12.2100\"" $
        "31.12.2100" @=? show (D XXXI Dez 2100),
      testCase "D9 - show (D XXXI Aug 2100) ->> \"31.8.2100\"" $
        "31.8.2100" @=? show (D XXXI Aug 2100),
      testCase "D10 - show (D XXXI Sep 2100) ->> \"Datum ungueltig\"" $
        "Datum ungueltig" @=? show (D XXXI Sep 2100),
      testCase "D11 - show (D XXXI Jun 2100) ->> \"Datum ungueltig\"" $
        "Datum ungueltig" @=? show (D XXXI Jun 2100),

      testCase "D12 - show (D IV Sep 9) ->> \"4.9.9\"" $
        "4.9.9" @=? show (D IV Sep 9),
      testCase "D13 - show (D XIX Aug 14) ->> \"19.8.14\"" $
        "19.8.14" @=? show (D XIX Aug 14),
      testCase "D14 - show (D XXV Dez 800) ->> \"25.12.800\"" $
        "25.12.800" @=? show (D XXV Dez 800)
    ]
