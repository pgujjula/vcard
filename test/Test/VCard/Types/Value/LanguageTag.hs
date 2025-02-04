-- SPDX-FileCopyrightText: Copyright Preetham Gujjula
-- SPDX-License-Identifier: BSD-3-Clause
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Test.VCard.Types.Value.LanguageTag (tests) where

import Control.Monad (forM_)
import Data.BCP47 (en, enGB, enGBTJP, enTJP, enUS, es, sw)
import Data.Proxy (Proxy (..))
import Data.Text (Text)
import Test.Tasty (TestName, TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))
import Test.Tasty.QuickCheck (testProperty, (===))
import VCard.Parse (HasParser, parse)
import VCard.Serialize (HasSerializer, serialize)
import VCard.Types.Value.LanguageTag (LanguageTag)
import Prelude hiding (Integer)

{-# ANN module ("HLint: ignore Use camelCase" :: String) #-}

tests :: TestTree
tests =
  testGroup
    "LanguageTag"
    [ testGroup
        "LanguageTag"
        [ test_LanguageTag_parse,
          test_LanguageTag_serialize,
          test_LanguageTag_parse_serialize_roundtrip
        ]
    ]

test_LanguageTag_parse :: TestTree
test_LanguageTag_parse =
  testGroup
    "parse"
    [ testGroup
        "unit"
        [ testParseValid units_LanguageTag_valid,
          testParseInvalid (Proxy @LanguageTag) units_LanguageTag_invalid
        ]
    ]

test_LanguageTag_parse_serialize_roundtrip :: TestTree
test_LanguageTag_parse_serialize_roundtrip =
  testGroup
    "parse_serialize_roundtrip"
    [ testProperty "quickcheck" $ \(languageTag :: LanguageTag) ->
        let text = serialize languageTag
         in parse text === Just languageTag
    ]

test_LanguageTag_serialize :: TestTree
test_LanguageTag_serialize =
  testGroup
    "serialize"
    [ testSerialize "unit" units_LanguageTag_valid
    ]

units_LanguageTag_valid :: [(Text, LanguageTag)]
units_LanguageTag_valid =
  [ ("en", en),
    ("es", es),
    ("sw", sw),
    ("en-GB", enGB),
    ("en-US", enUS),
    ("en-t-jp", enTJP),
    ("en-GB-t-jp", enGBTJP)
  ]

units_LanguageTag_invalid :: [Text]
units_LanguageTag_invalid =
  [ " en",
    "\nen",
    "\r\nen",
    "en ",
    "en\n",
    "en\r\n"
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
