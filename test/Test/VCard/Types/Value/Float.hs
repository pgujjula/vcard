-- SPDX-FileCopyrightText: Copyright Preetham Gujjula
-- SPDX-License-Identifier: BSD-3-Clause
{-# LANGUAGE OverloadedStrings #-}

module Test.VCard.Types.Value.Float (tests) where

import Control.Monad (forM_)
import Data.Bifunctor (second)
import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.List.NonEmpty qualified as NonEmpty
import Data.Proxy (Proxy (..))
import Data.Scientific (Scientific, scientific)
import Data.Text (Text)
import Numeric.Natural (Natural)
import Test.Tasty (TestName, TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))
import VCard.Parse (HasParser, parse)
import VCard.Serialize (HasSerializer, serialize)
import VCard.Types.Value.Float
  ( Float (..),
    FloatList,
    NaturalLeadingZeros (..),
    fromScientific,
    toScientific,
  )
import VCard.Types.Value.List (List (..))
import VCard.Types.Value.Time (Sign (..))
import Prelude hiding (Float, Integer)

{-# ANN module ("HLint: ignore Use camelCase" :: String) #-}

tests :: TestTree
tests =
  testGroup
    "Float"
    [ test_Float,
      test_FloatList
    ]

test_Float :: TestTree
test_Float =
  testGroup
    "Float"
    [ test_Float_parse,
      test_Float_serialize,
      test_Float_toScientific,
      test_Float_fromScientific
    ]

test_Float_parse :: TestTree
test_Float_parse =
  testGroup
    "parse"
    [ testGroup
        "unit"
        [ testParseValid $
            map (\(text, float, _) -> (text, float)) units_Float_valid,
          testParseInvalid (Proxy @Float) units_Float_invalid
        ]
    ]

test_Float_serialize :: TestTree
test_Float_serialize =
  testGroup
    "serialize"
    [ testSerialize "unit" $
        map (\(text, float, _) -> (text, float)) units_Float_valid
    ]

test_Float_toScientific :: TestTree
test_Float_toScientific =
  testCase "toScientific" $ do
    forM_ units_Float_valid $ \(_, f, s) ->
      toScientific f @?= s
    forM_ units_Float_fromScientific $ \(s, f) ->
      toScientific f @?= s

test_Float_fromScientific :: TestTree
test_Float_fromScientific =
  testCase "fromScientific" $
    forM_ units_Float_fromScientific $ \(s, f) ->
      fromScientific s @?= f

units_Float_valid :: [(Text, Float, Scientific)]
units_Float_valid =
  [ ( "0",
      Float Nothing (nlz 0 0) Nothing,
      scientific 0 0
    ),
    ( "+0.0",
      Float (Just Plus) (nlz 0 0) (Just (nlz 0 0)),
      scientific 0 0
    ),
    ("-00.000", Float (Just Minus) (nlz 1 0) (Just (nlz 2 0)), scientific 0 0),
    --
    ("21", Float Nothing (nlz 0 21) Nothing, scientific 21 0),
    ("+021", Float (Just Plus) (nlz 1 21) Nothing, scientific 21 0),
    ("-2100", Float (Just Minus) (nlz 0 2100) Nothing, scientific (-21) 2),
    ("00210", Float Nothing (nlz 2 210) Nothing, scientific 21 1),
    --
    ( "103.406",
      Float Nothing (nlz 0 103) (Just (nlz 0 406)),
      scientific 103406 (-3)
    ),
    ( "+0103.406",
      Float (Just Plus) (nlz 1 103) (Just (nlz 0 406)),
      scientific 103406 (-3)
    ),
    ( "-10300.406",
      Float (Just Minus) (nlz 0 10300) (Just (nlz 0 406)),
      scientific (-10300406) (-3)
    ),
    ( "0001030.406",
      Float Nothing (nlz 3 1030) (Just (nlz 0 406)),
      scientific 1030406 (-3)
    ),
    ( "+103.00406",
      Float (Just Plus) (nlz 0 103) (Just (nlz 2 406)),
      scientific 10300406 (-5)
    ),
    ( "-000103.0406",
      Float (Just Minus) (nlz 3 103) (Just (nlz 1 406)),
      scientific (-1030406) (-4)
    ),
    ( "10300.000406",
      Float Nothing (nlz 0 10300) (Just (nlz 3 406)),
      scientific 10300000406 (-6)
    ),
    ( "+010300.000406",
      Float (Just Plus) (nlz 1 10300) (Just (nlz 3 406)),
      scientific 10300000406 (-6)
    ),
    ( "-103.4060",
      Float (Just Minus) (nlz 0 103) (Just (nlz 0 4060)),
      scientific (-1034060) (-4)
    ),
    ( "00103.406000",
      Float Nothing (nlz 2 103) (Just (nlz 0 406000)),
      scientific 103406000 (-6)
    ),
    ( "+1030.40600",
      Float (Just Plus) (nlz 0 1030) (Just (nlz 0 40600)),
      scientific 103040600 (-5)
    ),
    ( "-0001030.40600",
      Float (Just Minus) (nlz 3 1030) (Just (nlz 0 40600)),
      scientific (-103040600) (-5)
    ),
    ( "103.0004060",
      Float Nothing (nlz 0 103) (Just (nlz 3 4060)),
      scientific 103000406 (-6)
    ),
    ( "+00103.0004060",
      Float (Just Plus) (nlz 2 103) (Just (nlz 3 4060)),
      scientific 103000406 (-6)
    ),
    ( "-10300.0004060",
      Float (Just Minus) (nlz 0 10300) (Just (nlz 3 4060)),
      scientific (-10300000406) (-6)
    ),
    ( "00103000.040600",
      Float Nothing (nlz 2 103000) (Just (nlz 1 40600)),
      scientific 1030000406 (-4)
    ),
    --
    ( "+00003741434101568639131665736242662561279331957871681316708024145082",
      Float
        (Just Plus)
        ( nlz
            4
            3741434101568639131665736242662561279331957871681316708024145082
        )
        Nothing,
      scientific
        3741434101568639131665736242662561279331957871681316708024145082
        0
    ),
    ( "-00003741434101568639131665736242662561279331957871681316708024145082."
        <> "005329695021262933547326704056645049435771930394930926347191006575",
      Float
        (Just Minus)
        (nlz 4 3741434101568639131665736242662561279331957871681316708024145082)
        ( Just
            ( nlz
                2
                5329695021262933547326704056645049435771930394930926347191006575
            )
        ),
      scientific
        (-3741434101568639131665736242662561279331957871681316708024145082005329695021262933547326704056645049435771930394930926347191006575)
        (-66)
    )
  ]

units_Float_invalid :: [Text]
units_Float_invalid =
  [ ".",
    "+.",
    "-.",
    "123.",
    "+123.",
    "-123.",
    ".123",
    "+.123",
    "-.123",
    "++1",
    "1+1",
    "1e3"
  ]

units_Float_fromScientific :: [(Scientific, Float)]
units_Float_fromScientific =
  [ --
    -- Positives
    --
    (scientific 0 0, Float Nothing (nlz 0 0) Nothing),
    (scientific 0 1, Float Nothing (nlz 0 0) Nothing),
    (scientific 0 2, Float Nothing (nlz 0 0) Nothing),
    (scientific 0 3, Float Nothing (nlz 0 0) Nothing),
    (scientific 0 (-1), Float Nothing (nlz 0 0) Nothing),
    (scientific 0 (-2), Float Nothing (nlz 0 0) Nothing),
    (scientific 0 (-3), Float Nothing (nlz 0 0) Nothing),
    --
    (scientific 1 0, Float Nothing (nlz 0 1) Nothing),
    (scientific 1 1, Float Nothing (nlz 0 10) Nothing),
    (scientific 1 2, Float Nothing (nlz 0 100) Nothing),
    (scientific 1 3, Float Nothing (nlz 0 1000) Nothing),
    (scientific 1 (-1), Float Nothing (nlz 0 0) (Just (nlz 0 1))),
    (scientific 1 (-2), Float Nothing (nlz 0 0) (Just (nlz 1 1))),
    (scientific 1 (-3), Float Nothing (nlz 0 0) (Just (nlz 2 1))),
    (scientific 1 (-4), Float Nothing (nlz 0 0) (Just (nlz 3 1))),
    (scientific 1 (-5), Float Nothing (nlz 0 0) (Just (nlz 4 1))),
    --
    (scientific 5 0, Float Nothing (nlz 0 5) Nothing),
    (scientific 5 1, Float Nothing (nlz 0 50) Nothing),
    (scientific 5 2, Float Nothing (nlz 0 500) Nothing),
    (scientific 5 3, Float Nothing (nlz 0 5000) Nothing),
    (scientific 5 (-1), Float Nothing (nlz 0 0) (Just (nlz 0 5))),
    (scientific 5 (-2), Float Nothing (nlz 0 0) (Just (nlz 1 5))),
    (scientific 5 (-3), Float Nothing (nlz 0 0) (Just (nlz 2 5))),
    (scientific 5 (-4), Float Nothing (nlz 0 0) (Just (nlz 3 5))),
    (scientific 5 (-5), Float Nothing (nlz 0 0) (Just (nlz 4 5))),
    --
    (scientific 10 0, Float Nothing (nlz 0 10) Nothing),
    (scientific 10 1, Float Nothing (nlz 0 100) Nothing),
    (scientific 10 2, Float Nothing (nlz 0 1000) Nothing),
    (scientific 10 3, Float Nothing (nlz 0 10000) Nothing),
    (scientific 10 (-1), Float Nothing (nlz 0 1) Nothing),
    (scientific 10 (-2), Float Nothing (nlz 0 0) (Just (nlz 0 1))),
    (scientific 10 (-3), Float Nothing (nlz 0 0) (Just (nlz 1 1))),
    (scientific 10 (-4), Float Nothing (nlz 0 0) (Just (nlz 2 1))),
    (scientific 10 (-5), Float Nothing (nlz 0 0) (Just (nlz 3 1))),
    --
    (scientific 123 0, Float Nothing (nlz 0 123) Nothing),
    (scientific 123 1, Float Nothing (nlz 0 1230) Nothing),
    (scientific 123 2, Float Nothing (nlz 0 12300) Nothing),
    (scientific 123 3, Float Nothing (nlz 0 123000) Nothing),
    (scientific 123 (-1), Float Nothing (nlz 0 12) (Just (nlz 0 3))),
    (scientific 123 (-2), Float Nothing (nlz 0 1) (Just (nlz 0 23))),
    (scientific 123 (-3), Float Nothing (nlz 0 0) (Just (nlz 0 123))),
    (scientific 123 (-4), Float Nothing (nlz 0 0) (Just (nlz 1 123))),
    (scientific 123 (-5), Float Nothing (nlz 0 0) (Just (nlz 2 123))),
    --
    (scientific 4050 0, Float Nothing (nlz 0 4050) Nothing),
    (scientific 4050 1, Float Nothing (nlz 0 40500) Nothing),
    (scientific 4050 2, Float Nothing (nlz 0 405000) Nothing),
    (scientific 4050 3, Float Nothing (nlz 0 4050000) Nothing),
    (scientific 4050 (-1), Float Nothing (nlz 0 405) Nothing),
    (scientific 4050 (-2), Float Nothing (nlz 0 40) (Just (nlz 0 5))),
    (scientific 4050 (-3), Float Nothing (nlz 0 4) (Just (nlz 1 5))),
    (scientific 4050 (-4), Float Nothing (nlz 0 0) (Just (nlz 0 405))),
    (scientific 4050 (-5), Float Nothing (nlz 0 0) (Just (nlz 1 405))),
    (scientific 4050 (-6), Float Nothing (nlz 0 0) (Just (nlz 2 405))),
    (scientific 4050 (-7), Float Nothing (nlz 0 0) (Just (nlz 3 405))),
    --
    (scientific 6700800 0, Float Nothing (nlz 0 6700800) Nothing),
    (scientific 6700800 1, Float Nothing (nlz 0 67008000) Nothing),
    (scientific 6700800 2, Float Nothing (nlz 0 670080000) Nothing),
    (scientific 6700800 3, Float Nothing (nlz 0 6700800000) Nothing),
    (scientific 6700800 (-1), Float Nothing (nlz 0 670080) Nothing),
    (scientific 6700800 (-2), Float Nothing (nlz 0 67008) Nothing),
    (scientific 6700800 (-3), Float Nothing (nlz 0 6700) (Just (nlz 0 8))),
    (scientific 6700800 (-4), Float Nothing (nlz 0 670) (Just (nlz 1 8))),
    (scientific 6700800 (-5), Float Nothing (nlz 0 67) (Just (nlz 2 8))),
    (scientific 6700800 (-6), Float Nothing (nlz 0 6) (Just (nlz 0 7008))),
    (scientific 6700800 (-7), Float Nothing (nlz 0 0) (Just (nlz 0 67008))),
    (scientific 6700800 (-8), Float Nothing (nlz 0 0) (Just (nlz 1 67008))),
    (scientific 6700800 (-9), Float Nothing (nlz 0 0) (Just (nlz 2 67008))),
    --
    -- Negatives
    --
    (scientific (-1) 0, Float (Just Minus) (nlz 0 1) Nothing),
    (scientific (-1) 1, Float (Just Minus) (nlz 0 10) Nothing),
    (scientific (-1) 2, Float (Just Minus) (nlz 0 100) Nothing),
    (scientific (-1) 3, Float (Just Minus) (nlz 0 1000) Nothing),
    (scientific (-1) (-1), Float (Just Minus) (nlz 0 0) (Just (nlz 0 1))),
    (scientific (-1) (-2), Float (Just Minus) (nlz 0 0) (Just (nlz 1 1))),
    (scientific (-1) (-3), Float (Just Minus) (nlz 0 0) (Just (nlz 2 1))),
    (scientific (-1) (-4), Float (Just Minus) (nlz 0 0) (Just (nlz 3 1))),
    (scientific (-1) (-5), Float (Just Minus) (nlz 0 0) (Just (nlz 4 1))),
    --
    (scientific (-5) 0, Float (Just Minus) (nlz 0 5) Nothing),
    (scientific (-5) 1, Float (Just Minus) (nlz 0 50) Nothing),
    (scientific (-5) 2, Float (Just Minus) (nlz 0 500) Nothing),
    (scientific (-5) 3, Float (Just Minus) (nlz 0 5000) Nothing),
    (scientific (-5) (-1), Float (Just Minus) (nlz 0 0) (Just (nlz 0 5))),
    (scientific (-5) (-2), Float (Just Minus) (nlz 0 0) (Just (nlz 1 5))),
    (scientific (-5) (-3), Float (Just Minus) (nlz 0 0) (Just (nlz 2 5))),
    (scientific (-5) (-4), Float (Just Minus) (nlz 0 0) (Just (nlz 3 5))),
    (scientific (-5) (-5), Float (Just Minus) (nlz 0 0) (Just (nlz 4 5))),
    --
    (scientific (-10) 0, Float (Just Minus) (nlz 0 10) Nothing),
    (scientific (-10) 1, Float (Just Minus) (nlz 0 100) Nothing),
    (scientific (-10) 2, Float (Just Minus) (nlz 0 1000) Nothing),
    (scientific (-10) 3, Float (Just Minus) (nlz 0 10000) Nothing),
    (scientific (-10) (-1), Float (Just Minus) (nlz 0 1) Nothing),
    (scientific (-10) (-2), Float (Just Minus) (nlz 0 0) (Just (nlz 0 1))),
    (scientific (-10) (-3), Float (Just Minus) (nlz 0 0) (Just (nlz 1 1))),
    (scientific (-10) (-4), Float (Just Minus) (nlz 0 0) (Just (nlz 2 1))),
    (scientific (-10) (-5), Float (Just Minus) (nlz 0 0) (Just (nlz 3 1))),
    --
    (scientific (-123) 0, Float (Just Minus) (nlz 0 123) Nothing),
    (scientific (-123) 1, Float (Just Minus) (nlz 0 1230) Nothing),
    (scientific (-123) 2, Float (Just Minus) (nlz 0 12300) Nothing),
    (scientific (-123) 3, Float (Just Minus) (nlz 0 123000) Nothing),
    (scientific (-123) (-1), Float (Just Minus) (nlz 0 12) (Just (nlz 0 3))),
    (scientific (-123) (-2), Float (Just Minus) (nlz 0 1) (Just (nlz 0 23))),
    (scientific (-123) (-3), Float (Just Minus) (nlz 0 0) (Just (nlz 0 123))),
    (scientific (-123) (-4), Float (Just Minus) (nlz 0 0) (Just (nlz 1 123))),
    (scientific (-123) (-5), Float (Just Minus) (nlz 0 0) (Just (nlz 2 123))),
    --
    (scientific (-4050) 0, Float (Just Minus) (nlz 0 4050) Nothing),
    (scientific (-4050) 1, Float (Just Minus) (nlz 0 40500) Nothing),
    (scientific (-4050) 2, Float (Just Minus) (nlz 0 405000) Nothing),
    (scientific (-4050) 3, Float (Just Minus) (nlz 0 4050000) Nothing),
    (scientific (-4050) (-1), Float (Just Minus) (nlz 0 405) Nothing),
    (scientific (-4050) (-2), Float (Just Minus) (nlz 0 40) (Just (nlz 0 5))),
    (scientific (-4050) (-3), Float (Just Minus) (nlz 0 4) (Just (nlz 1 5))),
    (scientific (-4050) (-4), Float (Just Minus) (nlz 0 0) (Just (nlz 0 405))),
    (scientific (-4050) (-5), Float (Just Minus) (nlz 0 0) (Just (nlz 1 405))),
    (scientific (-4050) (-6), Float (Just Minus) (nlz 0 0) (Just (nlz 2 405))),
    (scientific (-4050) (-7), Float (Just Minus) (nlz 0 0) (Just (nlz 3 405))),
    --
    (scientific (-6700800) 0, Float (Just Minus) (nlz 0 6700800) Nothing),
    (scientific (-6700800) 1, Float (Just Minus) (nlz 0 67008000) Nothing),
    (scientific (-6700800) 2, Float (Just Minus) (nlz 0 670080000) Nothing),
    (scientific (-6700800) 3, Float (Just Minus) (nlz 0 6700800000) Nothing),
    (scientific (-6700800) (-1), Float (Just Minus) (nlz 0 670080) Nothing),
    (scientific (-6700800) (-2), Float (Just Minus) (nlz 0 67008) Nothing),
    (scientific (-6700800) (-3), Float (Just Minus) (nlz 0 6700) (Just (nlz 0 8))),
    (scientific (-6700800) (-4), Float (Just Minus) (nlz 0 670) (Just (nlz 1 8))),
    (scientific (-6700800) (-5), Float (Just Minus) (nlz 0 67) (Just (nlz 2 8))),
    (scientific (-6700800) (-6), Float (Just Minus) (nlz 0 6) (Just (nlz 0 7008))),
    (scientific (-6700800) (-7), Float (Just Minus) (nlz 0 0) (Just (nlz 0 67008))),
    (scientific (-6700800) (-8), Float (Just Minus) (nlz 0 0) (Just (nlz 1 67008))),
    (scientific (-6700800) (-9), Float (Just Minus) (nlz 0 0) (Just (nlz 2 67008)))
  ]

nlz :: Word -> Natural -> NaturalLeadingZeros
nlz = NaturalLeadingZeros

test_FloatList :: TestTree
test_FloatList =
  testGroup
    "FloatList"
    [ test_FloatList_parse,
      test_FloatList_serialize
    ]

test_FloatList_parse :: TestTree
test_FloatList_parse =
  testGroup
    "parse"
    [ testGroup
        "unit"
        [ testParseValid units_FloatList_valid,
          testParseInvalid (Proxy @FloatList) units_FloatList_invalid
        ]
    ]

test_FloatList_serialize :: TestTree
test_FloatList_serialize =
  testGroup
    "serialize"
    [ testSerialize "unit" units_FloatList_valid
    ]

units_FloatList_valid :: [(Text, FloatList)]
units_FloatList_valid =
  map
    (second List)
    [ -- singletons
      ("0", NonEmpty.singleton $ Float Nothing (nlz 0 0) Nothing),
      ("+021", NonEmpty.singleton $ Float (Just Plus) (nlz 1 21) Nothing),
      ( "-000103.0406",
        NonEmpty.singleton $ Float (Just Minus) (nlz 3 103) (Just (nlz 1 406))
      ),
      -- multiple
      ( "0,+021,-000103.0406",
        Float Nothing (nlz 0 0) Nothing
          :| [ Float (Just Plus) (nlz 1 21) Nothing,
               Float (Just Minus) (nlz 3 103) (Just (nlz 1 406))
             ]
      )
    ]

units_FloatList_invalid :: [Text]
units_FloatList_invalid =
  [ -- leading/trailing whitespace
    " +021,-000103.0406",
    "\n+021,-000103.0406",
    "\r\n+021,-000103.0406",
    "+021,-000103.0406 ",
    "+021,-000103.0406\n",
    "+021,-000103.0406\r\n",
    -- whitespace between entries
    "+021 ,-000103.0406",
    "+021\n,-000103.0406",
    "+021\r\n,-000103.0406",
    "+021, -000103.0406",
    "+021,\n-000103.0406",
    "+021,\r\n-000103.0406",
    -- empty strings/extraneous leading or trailing commas
    "",
    ",",
    ",,",
    "+021,",
    ",+021",
    "+021,-000103.0406,",
    ",+021,-000103.0406",
    -- strange constructions
    ".",
    "+.",
    "-.",
    "123.",
    "+123.",
    "-123.",
    ".123",
    "+.123",
    "-.123",
    "++1",
    "1+1",
    "1e3"
  ]

--------------------
-- Testing functions
--------------------

testParseValid :: (Show a, Eq a, HasParser a) => [(Text, a)] -> TestTree
testParseValid cases =
  testCase "valid" $
    forM_ cases $ \(text, value) ->
      parse text @?= Just value

testParseInvalid ::
  forall a.
  (Show a, Eq a, HasParser a) =>
  Proxy a ->
  [Text] ->
  TestTree
testParseInvalid _ cases =
  testCase "invalid" $
    forM_ cases $ \text ->
      parse text @?= (Nothing :: Maybe a)

testSerialize :: (HasSerializer a) => TestName -> [(Text, a)] -> TestTree
testSerialize name cases =
  testCase name $
    forM_ cases $ \(text, value) ->
      serialize value @?= text
