-- SPDX-FileCopyrightText: Copyright Preetham Gujjula
-- SPDX-License-Identifier: BSD-3-Clause
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module Test.VCard.Types.Param.Tz (tests) where

import Control.Monad (forM_)
import Data.Proxy (Proxy (..))
import Data.Text (Text)
import Network.URI.Static
import Test.Tasty (TestName, TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))
import VCard.Parse (HasParser, parse)
import VCard.Serialize (HasSerializer, serialize)
import VCard.Symbol.Private (symbolSing)
import VCard.Types.Param.Generic (Param (..))
import VCard.Types.Param.ParamValue (SParamValue (..), paramValueVal)
import VCard.Types.Param.Tz (Tz, TzValue (..))
import VCard.Types.Textual.Private.CaseInsensitive (CaseInsensitiveUpper (..))
import Vary qualified (from)

{-# ANN module ("HLint: ignore Use camelCase" :: String) #-}

tests :: TestTree
tests =
  testGroup
    "Tz"
    [ test_parse,
      test_serialize
    ]

test_parse :: TestTree
test_parse =
  testGroup
    "parse"
    [ testParseValid cases_valid,
      testParseInvalid (Proxy @Tz) cases_invalid
    ]

test_serialize :: TestTree
test_serialize = testSerialize "serialize" cases_valid

cases_valid :: [(Text, Tz)]
cases_valid =
  [ ( "TZ=",
      Param
        { paramName = CaseInsensitiveUpper (symbolSing @"TZ"),
          paramValue =
            TzValue (Vary.from (paramValueVal (SParamValue (symbolSing @""))))
        }
    ),
    ( "TZ=\"\"",
      Param
        { paramName = CaseInsensitiveUpper (symbolSing @"TZ"),
          paramValue =
            TzValue
              (Vary.from (paramValueVal (SParamValue (symbolSing @"\"\""))))
        }
    ),
    ( "TZ=foo",
      Param
        { paramName = CaseInsensitiveUpper (symbolSing @"TZ"),
          paramValue =
            TzValue
              (Vary.from (paramValueVal (SParamValue (symbolSing @"foo"))))
        }
    ),
    ( "TZ=\"foo\"",
      Param
        { paramName = CaseInsensitiveUpper (symbolSing @"TZ"),
          paramValue =
            TzValue
              (Vary.from (paramValueVal (SParamValue (symbolSing @"\"foo\""))))
        }
    ),
    ( "TZ=\"https://example.com/\"",
      Param
        { paramName = CaseInsensitiveUpper (symbolSing @"TZ"),
          paramValue =
            TzValue (Vary.from $$(staticURI "https://example.com/"))
        }
    )
  ]

cases_invalid :: [Text]
cases_invalid =
  [ "TZ=\"",
    "TZ=\"foo",
    "TZ=foo\"",
    "TZ=foo,bar",
    "TZ=https://example.com"
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
