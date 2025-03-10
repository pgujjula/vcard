-- SPDX-FileCopyrightText: Copyright Preetham Gujjula
-- SPDX-License-Identifier: BSD-3-Clause
{-# LANGUAGE OverloadedStrings #-}

module Test.VCard.Types.Value.Boolean (tests) where

import Control.Monad (forM_)
import Data.Proxy (Proxy (..))
import Data.Text (Text)
import Test.Tasty (TestName, TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))
import VCard.Parse (HasParser, parse)
import VCard.Serialize (HasSerializer, serialize)
import VCard.Types.Value.Boolean (Boolean (..), mkBoolean)

{-# ANN module ("HLint: ignore Use camelCase" :: String) #-}

tests :: TestTree
tests =
  testGroup
    "Boolean"
    [ test_Boolean_parse,
      test_Boolean_serialize,
      test_Boolean_mkBoolean
    ]

test_Boolean_parse :: TestTree
test_Boolean_parse =
  testGroup
    "parse"
    [ testGroup
        "unit"
        [ testParseValid units_Boolean_valid,
          testParseInvalid (Proxy @Boolean) units_Boolean_invalid
        ]
    ]

test_Boolean_serialize :: TestTree
test_Boolean_serialize =
  testGroup
    "serialize"
    [ testSerialize "unit" units_Boolean_valid
    ]

test_Boolean_mkBoolean :: TestTree
test_Boolean_mkBoolean =
  testGroup
    "mkBoolean"
    [ test_Boolean_mkBoolean_valid,
      test_Boolean_mkBoolean_invalid
    ]

test_Boolean_mkBoolean_valid :: TestTree
test_Boolean_mkBoolean_valid =
  testCase "valid" $
    forM_ units_Boolean_valid $ \(text, boolean) ->
      mkBoolean text @?= Just boolean

test_Boolean_mkBoolean_invalid :: TestTree
test_Boolean_mkBoolean_invalid =
  testCase "invalid" $
    forM_ units_Boolean_invalid $ \text ->
      mkBoolean text @?= Nothing

-- See also: units_LocalTime_valid_Hour
units_Boolean_valid :: [(Text, Boolean)]
units_Boolean_valid =
  [ ("TRUE", Boolean True "TRUE"),
    ("True", Boolean True "True"),
    ("true", Boolean True "true"),
    ("trUe", Boolean True "trUe"),
    ("FALSE", Boolean False "FALSE"),
    ("False", Boolean False "False"),
    ("false", Boolean False "false"),
    ("faLse", Boolean False "faLse")
  ]

units_Boolean_invalid :: [Text]
units_Boolean_invalid =
  [ -- whitespace
    " TRUE",
    "\nTRUE",
    "\r\nTRUE",
    "TRUE ",
    "TRUE\n",
    "TRUE\r\n",
    -- incomplete
    "TRU",
    "ALSE"
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
