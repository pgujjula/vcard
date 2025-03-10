-- SPDX-FileCopyrightText: Copyright Preetham Gujjula
-- SPDX-License-Identifier: BSD-3-Clause
{-# LANGUAGE OverloadedStrings #-}

module Test.VCard.AlphaDigitDash (tests) where

import Control.Monad (forM_)
import Data.Maybe (isJust, isNothing)
import Data.Proxy (Proxy (..))
import Data.Text (Text)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (Assertion, assertBool, testCase, (@?=))
import Test.VCard.AlphaDigitDash.TypeTests
  ( test_AlphaDigitDashLowerSymbol,
    test_AlphaDigitDashSymbol,
    test_AlphaDigitDashUpperSymbol,
  )
import VCard.AlphaDigitDash
  ( SomeAlphaDigitDashLowerSymbol (..),
    SomeAlphaDigitDashSymbol (..),
    SomeAlphaDigitDashUpperSymbol (..),
    testAlphaDigitDashLowerSymbol,
    testAlphaDigitDashSymbol,
    testAlphaDigitDashUpperSymbol,
  )
import VCard.Parse (HasParser, parse)
import VCard.Serialize (HasSerializer, serialize)
import VCard.Symbol.Private (symbolSing)

{-# ANN module ("HLint: ignore Use camelCase" :: String) #-}

tests :: TestTree
tests =
  testGroup
    "AlphaDigitDash"
    [ test_AlphaDigitDashSymbol,
      test_testAlphaDigitDashSymbol,
      test_SomeAlphaDigitDashSymbol,
      test_AlphaDigitDashLowerSymbol,
      test_testAlphaDigitDashLowerSymbol,
      test_SomeAlphaDigitDashLowerSymbol,
      test_AlphaDigitDashUpperSymbol,
      test_testAlphaDigitDashUpperSymbol,
      test_SomeAlphaDigitDashUpperSymbol
    ]

assertIsJust :: Maybe a -> Assertion
assertIsJust x = assertBool "expected Just" (isJust x)

assertIsNothing :: Maybe a -> Assertion
assertIsNothing x = assertBool "expected Nothing" (isNothing x)

test_testAlphaDigitDashSymbol :: TestTree
test_testAlphaDigitDashSymbol =
  testCase "testAlphaDigitDashSymbol" $ do
    assertIsJust (testAlphaDigitDashSymbol (symbolSing @"a"))
    assertIsJust (testAlphaDigitDashSymbol (symbolSing @"foo-BAR-123"))
    assertIsJust
      ( testAlphaDigitDashSymbol
          ( symbolSing
              @"abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789-"
          )
      )
    --
    assertIsNothing (testAlphaDigitDashSymbol (symbolSing @""))
    assertIsNothing (testAlphaDigitDashSymbol (symbolSing @"foo-BAR-!23"))

test_SomeAlphaDigitDashSymbol :: TestTree
test_SomeAlphaDigitDashSymbol =
  testGroup
    "SomeAlphaDigitDashSymbol"
    [ test_SomeAlphaDigitDashSymbol_parse,
      test_SomeAlphaDigitDashSymbol_serialize
    ]

test_SomeAlphaDigitDashSymbol_parse :: TestTree
test_SomeAlphaDigitDashSymbol_parse =
  testGroup
    "parse"
    [ testParseValid cases_SomeAlphaDigitDashSymbol_valid,
      testParseInvalid
        (Proxy @SomeAlphaDigitDashSymbol)
        cases_SomeAlphaDigitDashSymbol_invalid
    ]

test_SomeAlphaDigitDashSymbol_serialize :: TestTree
test_SomeAlphaDigitDashSymbol_serialize =
  testSerialize cases_SomeAlphaDigitDashSymbol_valid

cases_SomeAlphaDigitDashSymbol_valid :: [(Text, SomeAlphaDigitDashSymbol)]
cases_SomeAlphaDigitDashSymbol_valid =
  [ ("a", SomeAlphaDigitDashSymbol (symbolSing @"a")),
    ("foo-BAR-123", SomeAlphaDigitDashSymbol (symbolSing @"foo-BAR-123")),
    ( "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789-",
      SomeAlphaDigitDashSymbol
        ( symbolSing
            @"abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789-"
        )
    )
  ]

cases_SomeAlphaDigitDashSymbol_invalid :: [Text]
cases_SomeAlphaDigitDashSymbol_invalid = ["", "foo-BAR-!23"]

test_testAlphaDigitDashLowerSymbol :: TestTree
test_testAlphaDigitDashLowerSymbol =
  testCase "testAlphaDigitDashLowerSymbol" $ do
    assertIsJust (testAlphaDigitDashLowerSymbol (symbolSing @"a"))
    assertIsJust (testAlphaDigitDashLowerSymbol (symbolSing @"foo-bar-123"))
    assertIsJust
      ( testAlphaDigitDashLowerSymbol
          ( symbolSing
              @"abcdefghijklmnopqrstuvwxyz0123456789-"
          )
      )
    --
    assertIsNothing (testAlphaDigitDashLowerSymbol (symbolSing @""))
    assertIsNothing (testAlphaDigitDashLowerSymbol (symbolSing @"foo-bAr-123"))
    assertIsNothing (testAlphaDigitDashLowerSymbol (symbolSing @"foo-bar-!23"))

test_SomeAlphaDigitDashLowerSymbol :: TestTree
test_SomeAlphaDigitDashLowerSymbol =
  testGroup
    "SomeAlphaDigitDashLowerSymbol"
    [ test_SomeAlphaDigitDashLowerSymbol_parse,
      test_SomeAlphaDigitDashLowerSymbol_serialize
    ]

test_SomeAlphaDigitDashLowerSymbol_parse :: TestTree
test_SomeAlphaDigitDashLowerSymbol_parse =
  testGroup
    "parse"
    [ testParseValid cases_SomeAlphaDigitDashLowerSymbol_valid,
      testParseInvalid
        (Proxy @SomeAlphaDigitDashLowerSymbol)
        cases_SomeAlphaDigitDashLowerSymbol_invalid
    ]

test_SomeAlphaDigitDashLowerSymbol_serialize :: TestTree
test_SomeAlphaDigitDashLowerSymbol_serialize =
  testSerialize cases_SomeAlphaDigitDashLowerSymbol_valid

cases_SomeAlphaDigitDashLowerSymbol_valid ::
  [(Text, SomeAlphaDigitDashLowerSymbol)]
cases_SomeAlphaDigitDashLowerSymbol_valid =
  [ ("a", SomeAlphaDigitDashLowerSymbol (symbolSing @"a")),
    ("foo-bar-123", SomeAlphaDigitDashLowerSymbol (symbolSing @"foo-bar-123")),
    ( "abcdefghijklmnopqrstuvwxyz0123456789-",
      SomeAlphaDigitDashLowerSymbol
        (symbolSing @"abcdefghijklmnopqrstuvwxyz0123456789-")
    )
  ]

cases_SomeAlphaDigitDashLowerSymbol_invalid :: [Text]
cases_SomeAlphaDigitDashLowerSymbol_invalid = ["", "foo-bAr-123", "foo-bar-!23"]

test_testAlphaDigitDashUpperSymbol :: TestTree
test_testAlphaDigitDashUpperSymbol =
  testCase "testAlphaDigitDashUpperSymbol" $ do
    assertIsJust (testAlphaDigitDashUpperSymbol (symbolSing @"A"))
    assertIsJust (testAlphaDigitDashUpperSymbol (symbolSing @"FOO-BAR-123"))
    assertIsJust
      ( testAlphaDigitDashUpperSymbol
          ( symbolSing
              @"ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789-"
          )
      )
    --
    assertIsNothing (testAlphaDigitDashUpperSymbol (symbolSing @""))
    assertIsNothing (testAlphaDigitDashUpperSymbol (symbolSing @"FOO-bAR-123"))
    assertIsNothing (testAlphaDigitDashUpperSymbol (symbolSing @"FOO-BAR-!23"))

test_SomeAlphaDigitDashUpperSymbol :: TestTree
test_SomeAlphaDigitDashUpperSymbol =
  testGroup
    "SomeAlphaDigitDashUpperSymbol"
    [ test_SomeAlphaDigitDashUpperSymbol_parse,
      test_SomeAlphaDigitDashUpperSymbol_serialize
    ]

test_SomeAlphaDigitDashUpperSymbol_parse :: TestTree
test_SomeAlphaDigitDashUpperSymbol_parse =
  testGroup
    "parse"
    [ testParseValid cases_SomeAlphaDigitDashUpperSymbol_valid,
      testParseInvalid
        (Proxy @SomeAlphaDigitDashUpperSymbol)
        cases_SomeAlphaDigitDashUpperSymbol_invalid
    ]

test_SomeAlphaDigitDashUpperSymbol_serialize :: TestTree
test_SomeAlphaDigitDashUpperSymbol_serialize =
  testSerialize cases_SomeAlphaDigitDashUpperSymbol_valid

cases_SomeAlphaDigitDashUpperSymbol_valid ::
  [(Text, SomeAlphaDigitDashUpperSymbol)]
cases_SomeAlphaDigitDashUpperSymbol_valid =
  [ ("A", SomeAlphaDigitDashUpperSymbol (symbolSing @"A")),
    ("FOO-BAR-123", SomeAlphaDigitDashUpperSymbol (symbolSing @"FOO-BAR-123")),
    ( "ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789-",
      SomeAlphaDigitDashUpperSymbol
        (symbolSing @"ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789-")
    )
  ]

cases_SomeAlphaDigitDashUpperSymbol_invalid :: [Text]
cases_SomeAlphaDigitDashUpperSymbol_invalid = ["", "FOO-bAR-123", "FOO-BAR-!23"]

-- Utilities
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
