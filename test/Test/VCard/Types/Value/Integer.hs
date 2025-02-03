-- SPDX-FileCopyrightText: Copyright Preetham Gujjula
-- SPDX-License-Identifier: BSD-3-Clause
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Test.VCard.Types.Value.Integer (tests) where

import Control.Monad (forM_)
import Data.Finite (finite)
import Data.Int (Int64)
import Data.Proxy (Proxy (..))
import Data.Text (Text)
import Test.Tasty (TestName, TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))
import TextShow (showt)
import VCard.Parse (HasParser, parse)
import VCard.Serialize (HasSerializer, serialize)
import VCard.Types.Value.Integer (Integer (..), IntegerValue (..))
import Prelude hiding (Integer)

{-# ANN module ("HLint: ignore Use camelCase" :: String) #-}

tests :: TestTree
tests =
  testGroup
    "Integer"
    [ test_Integer_parse,
      test_Integer_serialize
    ]

test_Integer_parse :: TestTree
test_Integer_parse =
  testGroup
    "parse"
    [ testGroup
        "unit"
        [ testParseValid units_Integer_valid,
          testParseInvalid (Proxy @Integer) units_Integer_invalid
        ]
    ]

test_Integer_serialize :: TestTree
test_Integer_serialize =
  testGroup
    "serialize"
    [ testSerialize "unit" units_Integer_valid
    ]

units_Integer_valid :: [(Text, Integer)]
units_Integer_valid =
  [ -- bounds
    ( "9223372036854775807",
      Integer 0 (Unsigned (finite 9223372036854775807))
    ),
    ( "+9223372036854775807",
      Integer 0 (Positive (finite 9223372036854775807))
    ),
    ( "-9223372036854775808",
      Integer 0 (Negative (finite 9223372036854775808))
    ),
    ( showt (maxBound @Int64),
      Integer 0 (Unsigned (finite 9223372036854775807))
    ),
    ( showt (minBound @Int64),
      Integer 0 (Negative (finite 9223372036854775808))
    ),
    -- zero
    ("0", Integer 0 $ Unsigned 0),
    ("+0", Integer 0 $ Positive 0),
    ("-0", Integer 0 $ Negative 0),
    -- positives
    ("1", Integer 0 (Unsigned 1)),
    ("+1", Integer 0 (Positive 1)),
    ("25", Integer 0 (Unsigned 25)),
    ("+25", Integer 0 (Positive 25)),
    -- negatives
    ("-1", Integer 0 (Negative 1)),
    ("-25", Integer 0 (Negative 25)),
    -- leading zeros
    ("00", Integer 1 (Unsigned 0)),
    ("+000", Integer 2 (Positive 0)),
    ("-0000", Integer 3 (Negative 0)),
    ("01", Integer 1 (Unsigned 1)),
    ("+001", Integer 2 (Positive 1)),
    ("-0001", Integer 3 (Negative 1)),
    ("025", Integer 1 (Unsigned 25)),
    ("+0025", Integer 2 (Positive 25)),
    ("-00025", Integer 3 (Negative 25)),
    ( "000000000000000000000000000000000000000048",
      Integer 40 (Unsigned 48)
    ),
    ( "00000000000000000000000000000000000000009223372036854775807",
      Integer 40 (Unsigned 9223372036854775807)
    ),
    ( "+00000000000000000000000000000000000000009223372036854775807",
      Integer 40 (Positive 9223372036854775807)
    ),
    ( "-00000000000000000000000000000000000000009223372036854775808",
      Integer 40 (Negative 9223372036854775808)
    )
  ]

units_Integer_invalid :: [Text]
units_Integer_invalid =
  [ -- out of bounds
    "9223372036854775808",
    "+9223372036854775808",
    "-9223372036854775809",
    showt (toInteger (maxBound @Int64) + 1),
    showt (toInteger (minBound @Int64) - 1),
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
