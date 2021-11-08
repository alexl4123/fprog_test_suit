module TestSuite1 where

import Test.Tasty
import Test.Tasty.HUnit

import Angabe1

spec :: TestTree
spec = testGroup "Exercise 1" [a_1, a_2, a_3, a_4]

a_1 :: TestTree
a_1 =
  testGroup
    "ist_tzr"
    [ testCase "0 - ist_tzr \"Urknallexplosion\" \"knall\" ->> True" $
        ist_tzr "Urknallexplosion" "knall" @=? True,
      testCase "1 - ist_tzr \"Urknallexplosion\" \"Knall\" ->> False" $
        ist_tzr "Urknallexplosion" "Knall" @=? False,
      testCase "2 - ist_tzr \"Urknallexplosion\" \"\" ->> True" $
        ist_tzr "Urknallexplosion" "" @=? True,
      testCase "3 - ist_tzr \"\" \"Urknallexplosion\" ->> False" $
        ist_tzr "" "Urknallexplosion" @=? False,
      testCase "4 - ist_tzr \"Urknallexplosionsknall\" \"knall\" ->> True" $
        ist_tzr "Urknallexplosionsknall" "knall" @=? True
    ]

a_2 :: TestTree
a_2 =
  testGroup
    "tzr_zeuge"
    [ testCase "0 - tzr_zeuge \"Urknallexplosion\" \"knall\" ->> (\"Ur\",\"knall\",\"explosion\")" $
        ("Ur", "knall", "explosion") @=? tzr_zeuge "Urknallexplosion" "knall",
      testCase "1 - tzr_zeuge \"Urknallexplosion\" \"Knall\" ->> (\"\", \"KnallKnall\", \"\")" $
        ("","KnallKnall","") @=? tzr_zeuge "Urknallexplosion" "Knall",
      testCase "2 - tzr_zeuge \"Urknallexplosionsknall\" \"knall\" ->> (\"Ur\",\"knall\",\"explosionsknall\"),..." $
        ("Ur","knall","explosionsknall") @=? tzr_zeuge "Urknallexplosionsknall" "knall",
      testCase "3 - tzr_zeuge \"Urknall\" \"\" ->> (\"\",\"\",\"Urknall\"),..." $
        ("","","Urknall") @=? tzr_zeuge "Urknall" ""
    ]

a_3 :: TestTree
a_3 =
  testGroup
    "tzr_zeugen"
    [ testCase "0 - tzr_zeugen \"Urknallexplosion\" \"knall\" ->> [(\"Ur\",\"knall\",\"explosion\")]" $
        [("Ur","knall","explosion")] @=? tzr_zeugen "Urknallexplosion" "knall",
      testCase "1 - tzr_zeugen \"Urknallexplosion\" \"Knall\" ->> []" $
        [] @=? tzr_zeugen "Urknallexplosion" "Knall",
      testCase "2 - tzr_zeugen \"Urknallexplosionsknall\" \"knall\" ->> [(\"Ur\",\"knall\",\"explosionsknall\"),...]" $
        [("Ur","knall","explosionsknall"),("Urknallexplosions","knall","")] @=? tzr_zeugen "Urknallexplosionsknall" "knall",
      testCase "3 - tzr_zeugen \"Ur\" \"\" ->> [(\"\",\"\",\"Ur\"),...]" $
        [("","","Ur"),("U","","r"),("Ur","","")] @=? tzr_zeugen "Ur" ""
    ]

a_4 :: TestTree
a_4 =
  testGroup
    "wieOft"
    [ testCase "0 - wieOft \"Urknallexplosion\" \"knall\" ->> 1" $
        1 @=? wieOft "Urknallexplosion" "knall",
      testCase "1 - wieOft \"Urknallexplosion\" \"Knall\" ->> 0" $
        0 @=? wieOft "Urknallexplosion" "Knall",
      testCase "2 - wieOft \"Urknallexplosionsknall\" \"knall\" ->> 2" $
        2 @=? wieOft "Urknallexplosionsknall" "knall",
      testCase "3 - wieOft \"Ur\" \"\" ->> 3" $
        3 @=? wieOft "Ur" ""
    ]
