-- SPDX-FileCopyrightText: Copyright Preetham Gujjula
-- SPDX-License-Identifier: BSD-3-Clause
{-# LANGUAGE OverloadedStrings #-}

module Test.VCard.Types.Param.SortAs (tests) where

import Control.Monad (forM_)
import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.List.NonEmpty qualified as NonEmpty
import Data.Proxy (Proxy (..))
import Data.Text (Text)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))
import VCard.CaseInsensitive (CaseInsensitiveUpper (..))
import VCard.Parse (HasParser, parse)
import VCard.Serialize (HasSerializer, serialize)
import VCard.Symbol.Private (symbolSing)
import VCard.Types.Param.Generic (Param (..))
import VCard.Types.Param.ParamValue (SParamValue (..), paramValueVal)
import VCard.Types.Param.SortAs (SortAs)

{-# ANN module ("HLint: ignore Use camelCase" :: String) #-}

tests :: TestTree
tests =
  testGroup
    "SortAs"
    [ test_parse,
      test_serialize
    ]

test_parse :: TestTree
test_parse =
  testGroup
    "parse"
    [ testParseValid cases_valid,
      testParseInvalid (Proxy @SortAs) cases_invalid
    ]

test_serialize :: TestTree
test_serialize = testSerialize cases_valid

cases_valid :: [(Text, SortAs)]
cases_valid =
  [ ( "SORT-AS=foo",
      Param
        { paramName = CaseInsensitiveUpper (symbolSing @"SORT-AS"),
          paramValue = NonEmpty.singleton (paramValueVal (SParamValue (symbolSing @"foo")))
        }
    ),
    ( "SORT-AS= foo,,,\"bar,baz;\",qux",
      Param
        { paramName = CaseInsensitiveUpper (symbolSing @"SORT-AS"),
          paramValue =
            paramValueVal (SParamValue (symbolSing @" foo"))
              :| [ paramValueVal (SParamValue (symbolSing @"")),
                   paramValueVal (SParamValue (symbolSing @"")),
                   paramValueVal (SParamValue (symbolSing @"\"bar,baz;\"")),
                   paramValueVal (SParamValue (symbolSing @"qux"))
                 ]
        }
    )
  ]

cases_invalid :: [Text]
cases_invalid =
  [ "SORT-AS=:",
    "SORT-AS=foo;",
    "SORT-AS=\"",
    "SORT-AS=\"\"\"",
    "SORT-AS=\x00"
  ]

--
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

testSerialize :: (HasSerializer a) => [(Text, a)] -> TestTree
testSerialize cases =
  testCase "serialize" $
    forM_ cases $ \(text, value) ->
      serialize value @?= text
