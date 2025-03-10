-- SPDX-FileCopyrightText: Copyright Preetham Gujjula
-- SPDX-License-Identifier: BSD-3-Clause
{-# LANGUAGE OverloadedStrings #-}

module Test.VCard.Types.Value.Integer (tests) where

import Control.Monad (forM_)
import Data.Bifunctor (second)
import Data.Finite (finite)
import Data.Int (Int64)
import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.List.NonEmpty qualified as NonEmpty
import Data.Proxy (Proxy (..))
import Data.Text (Text)
import Test.Tasty (TestName, TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))
import VCard.Parse (HasParser, parse)
import VCard.Serialize (HasSerializer, serialize)
import VCard.Types.Value.Integer
  ( Integer (..),
    IntegerList,
    IntegerValue (..),
    fromInt64,
    toInt64,
  )
import VCard.Types.Value.List (List (..))
import VCard.Util (intToText)
import Prelude hiding (Integer)

{-# ANN module ("HLint: ignore Use camelCase" :: String) #-}

tests :: TestTree
tests =
  testGroup
    "Integer"
    [ testGroup
        "Integer"
        [ test_Integer_parse,
          test_Integer_serialize,
          test_Integer_toInt64,
          test_Integer_fromInt64
        ],
      testGroup
        "IntegerList"
        [ test_IntegerList_parse,
          test_IntegerList_serialize
        ]
    ]

test_Integer_parse :: TestTree
test_Integer_parse =
  testGroup
    "parse"
    [ testGroup
        "unit"
        [ testParseValid (map (\(a, b, _) -> (a, b)) units_Integer_valid),
          testParseInvalid (Proxy @Integer) units_Integer_invalid
        ]
    ]

test_Integer_serialize :: TestTree
test_Integer_serialize =
  testGroup
    "serialize"
    [ testSerialize "unit" (map (\(a, b, _) -> (a, b)) units_Integer_valid)
    ]

test_Integer_toInt64 :: TestTree
test_Integer_toInt64 =
  testCase "toInt64" $
    forM_ units_Integer_valid $ \(_, integer, int64) ->
      toInt64 integer @?= int64

test_Integer_fromInt64 :: TestTree
test_Integer_fromInt64 =
  testCase "fromInt64" $
    forM_ units_Integer_fromInt64 $ \(int64, integer) ->
      fromInt64 int64 @?= integer

units_Integer_valid :: [(Text, Integer, Int64)]
units_Integer_valid =
  [ -- bounds
    ( "9223372036854775807",
      Integer 0 (Unsigned (finite 9223372036854775807)),
      9223372036854775807
    ),
    ( "+9223372036854775807",
      Integer 0 (Positive (finite 9223372036854775807)),
      9223372036854775807
    ),
    ( "-9223372036854775808",
      Integer 0 (Negative (finite 9223372036854775808)),
      -9223372036854775808
    ),
    ( intToText (maxBound @Int64),
      Integer 0 (Unsigned (finite 9223372036854775807)),
      9223372036854775807
    ),
    ( intToText (minBound @Int64),
      Integer 0 (Negative (finite 9223372036854775808)),
      -9223372036854775808
    ),
    -- zero
    ("0", Integer 0 $ Unsigned 0, 0),
    ("+0", Integer 0 $ Positive 0, 0),
    ("-0", Integer 0 $ Negative 0, 0),
    -- positives
    ("1", Integer 0 (Unsigned 1), 1),
    ("+1", Integer 0 (Positive 1), 1),
    ("25", Integer 0 (Unsigned 25), 25),
    ("+25", Integer 0 (Positive 25), 25),
    -- negatives
    ("-1", Integer 0 (Negative 1), -1),
    ("-25", Integer 0 (Negative 25), -25),
    -- leading zeros
    ("00", Integer 1 (Unsigned 0), 0),
    ("+000", Integer 2 (Positive 0), 0),
    ("-0000", Integer 3 (Negative 0), 0),
    ("01", Integer 1 (Unsigned 1), 1),
    ("+001", Integer 2 (Positive 1), 1),
    ("-0001", Integer 3 (Negative 1), -1),
    ("025", Integer 1 (Unsigned 25), 25),
    ("+0025", Integer 2 (Positive 25), 25),
    ("-00025", Integer 3 (Negative 25), -25),
    ( "000000000000000000000000000000000000000048",
      Integer 40 (Unsigned 48),
      48
    ),
    ( "00000000000000000000000000000000000000009223372036854775807",
      Integer 40 (Unsigned 9223372036854775807),
      9223372036854775807
    ),
    ( "+00000000000000000000000000000000000000009223372036854775807",
      Integer 40 (Positive 9223372036854775807),
      9223372036854775807
    ),
    ( "-00000000000000000000000000000000000000009223372036854775808",
      Integer 40 (Negative 9223372036854775808),
      -9223372036854775808
    )
  ]

units_Integer_invalid :: [Text]
units_Integer_invalid =
  [ -- out of bounds
    "9223372036854775808",
    "+9223372036854775808",
    "-9223372036854775809",
    intToText (toInteger (maxBound @Int64) + 1),
    intToText (toInteger (minBound @Int64) - 1),
    "10000000000000000000",
    "009223372036854775808",
    "+009223372036854775808",
    "-009223372036854775809",
    -- invalid syntax
    "++5",
    "+-5",
    "-+5",
    "1e3",
    "1+1",
    "0x3",
    "2.0",
    " 5",
    "5 ",
    "\n5",
    "5\n",
    "\r\n5",
    "5\r\n"
  ]

units_Integer_fromInt64 :: [(Int64, Integer)]
units_Integer_fromInt64 =
  [ (maxBound, Integer 0 (Unsigned 9223372036854775807)),
    (25, Integer 0 (Unsigned 25)),
    (1, Integer 0 (Unsigned 1)),
    (0, Integer 0 (Unsigned 0)),
    (-1, Integer 0 (Negative 1)),
    (-25, Integer 0 (Negative 25)),
    (-9223372036854775807, Integer 0 (Negative 9223372036854775807)),
    (-9223372036854775808, Integer 0 (Negative 9223372036854775808))
  ]

test_IntegerList_parse :: TestTree
test_IntegerList_parse =
  testGroup
    "parse"
    [ testGroup
        "unit"
        [ testParseValid units_IntegerList_valid,
          testParseInvalid (Proxy @IntegerList) units_IntegerList_invalid
        ]
    ]

test_IntegerList_serialize :: TestTree
test_IntegerList_serialize =
  testGroup
    "serialize"
    [ testSerialize "unit" units_IntegerList_valid
    ]

units_IntegerList_valid :: [(Text, IntegerList)]
units_IntegerList_valid =
  map
    (second List)
    [ -- singletons
      ("+0", NonEmpty.singleton (Integer 0 (Positive 0))),
      ("-01", NonEmpty.singleton (Integer 1 (Negative 1))),
      ( "009223372036854775807",
        NonEmpty.singleton (Integer 2 (Unsigned 9223372036854775807))
      ),
      -- multiple
      ( "+0,-01,009223372036854775807",
        Integer 0 (Positive 0)
          :| [ Integer 1 (Negative 1),
               Integer 2 (Unsigned 9223372036854775807)
             ]
      )
    ]

units_IntegerList_invalid :: [Text]
units_IntegerList_invalid =
  [ -- out of bounds
    "+0,-01,9223372036854775808",
    -- leading trailing whitespace
    " +0,-01",
    "\n+0,-01",
    "\r\n+0,-01",
    "+0,-01 ",
    "+0,-01\n",
    "+0,-01\r\n",
    -- whitespace between entries
    "+0 ,-01",
    "+0\n,-01",
    "+0\r\n,-01",
    "+0, -01",
    "+0,\n-01",
    "+0,\r\n-01",
    -- empty strings/extraneous leading or trailing commas
    "",
    ",",
    ",,",
    "+0,",
    ",+0",
    "+0,-01,",
    ",+0,-01",
    -- strange constructions
    "++5",
    "+-5",
    "-+5",
    "1e3",
    "1+1",
    "0x3",
    "2.0"
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
