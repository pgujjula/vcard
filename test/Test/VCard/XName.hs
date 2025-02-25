-- SPDX-FileCopyrightText: Copyright Preetham Gujjula
-- SPDX-License-Identifier: BSD-3-Clause
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}

module Test.VCard.XName (tests) where

import Data.Maybe (isJust, isNothing)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (Assertion, assertBool, testCase)
import Test.VCard.XName.TypeTests
  ( test_XNameLowerSymbol,
    test_XNameSymbol,
    test_XNameUpperSymbol,
  )
import VCard.Symbol.Private (symbolSing)
import VCard.XName
  ( testXNameLowerSymbol,
    testXNameSymbol,
    testXNameUpperSymbol,
  )

tests :: TestTree
tests =
  testGroup
    "XName"
    [ test_XNameSymbol,
      test_testXNameSymbol,
      test_XNameLowerSymbol,
      test_testXNameLowerSymbol,
      test_XNameUpperSymbol,
      test_testXNameUpperSymbol
    ]

assertIsJust :: Maybe a -> Assertion
assertIsJust x = assertBool "expected Just" (isJust x)

assertIsNothing :: Maybe a -> Assertion
assertIsNothing x = assertBool "expected Nothing" (isNothing x)

test_testXNameSymbol :: TestTree
test_testXNameSymbol =
  testCase "testXNameSymbol" $ do
    assertIsJust (testXNameSymbol (symbolSing @"x-a"))
    assertIsJust (testXNameSymbol (symbolSing @"X-a"))
    assertIsJust (testXNameSymbol (symbolSing @"x-A"))
    assertIsJust (testXNameSymbol (symbolSing @"X-A"))
    assertIsJust (testXNameSymbol (symbolSing @"x-foo"))
    assertIsJust (testXNameSymbol (symbolSing @"X-foo"))
    assertIsJust (testXNameSymbol (symbolSing @"x-Foo"))
    assertIsJust (testXNameSymbol (symbolSing @"X-Foo"))
    assertIsJust (testXNameSymbol (symbolSing @"x-FOO"))
    assertIsJust (testXNameSymbol (symbolSing @"X-FOO"))
    --
    assertIsNothing (testXNameSymbol (symbolSing @""))
    assertIsNothing (testXNameSymbol (symbolSing @"a"))
    assertIsNothing (testXNameSymbol (symbolSing @"A"))
    assertIsNothing (testXNameSymbol (symbolSing @"foo"))
    assertIsNothing (testXNameSymbol (symbolSing @"Foo"))
    assertIsNothing (testXNameSymbol (symbolSing @"FOO"))
    assertIsNothing (testXNameSymbol (symbolSing @"x"))
    assertIsNothing (testXNameSymbol (symbolSing @"X"))
    assertIsNothing (testXNameSymbol (symbolSing @"x-"))
    assertIsNothing (testXNameSymbol (symbolSing @"X-"))

test_testXNameLowerSymbol :: TestTree
test_testXNameLowerSymbol =
  testCase "testXNameLowerSymbol" $ do
    assertIsJust (testXNameLowerSymbol (symbolSing @"x-a"))
    assertIsJust (testXNameLowerSymbol (symbolSing @"x-foo"))
    --
    assertIsNothing (testXNameLowerSymbol (symbolSing @"x-Foo"))
    assertIsNothing (testXNameLowerSymbol (symbolSing @"X-Foo"))
    assertIsNothing (testXNameLowerSymbol (symbolSing @"x-FOO"))
    assertIsNothing (testXNameLowerSymbol (symbolSing @"X-FOO"))
    assertIsNothing (testXNameLowerSymbol (symbolSing @"x-A"))
    assertIsNothing (testXNameLowerSymbol (symbolSing @"X-a"))
    assertIsNothing (testXNameLowerSymbol (symbolSing @"X-A"))
    assertIsNothing (testXNameLowerSymbol (symbolSing @"X-foo"))
    --
    assertIsNothing (testXNameLowerSymbol (symbolSing @""))
    assertIsNothing (testXNameLowerSymbol (symbolSing @"a"))
    assertIsNothing (testXNameLowerSymbol (symbolSing @"A"))
    assertIsNothing (testXNameLowerSymbol (symbolSing @"foo"))
    assertIsNothing (testXNameLowerSymbol (symbolSing @"Foo"))
    assertIsNothing (testXNameLowerSymbol (symbolSing @"FOO"))
    assertIsNothing (testXNameLowerSymbol (symbolSing @"x"))
    assertIsNothing (testXNameLowerSymbol (symbolSing @"X"))
    assertIsNothing (testXNameLowerSymbol (symbolSing @"x-"))
    assertIsNothing (testXNameLowerSymbol (symbolSing @"X-"))

test_testXNameUpperSymbol :: TestTree
test_testXNameUpperSymbol =
  testCase "testXNameUpperSymbol" $ do
    assertIsJust (testXNameUpperSymbol (symbolSing @"X-A"))
    assertIsJust (testXNameUpperSymbol (symbolSing @"X-FOO"))
    --
    assertIsNothing (testXNameUpperSymbol (symbolSing @"x-FOO"))
    assertIsNothing (testXNameUpperSymbol (symbolSing @"x-foo"))
    assertIsNothing (testXNameUpperSymbol (symbolSing @"X-foo"))
    assertIsNothing (testXNameUpperSymbol (symbolSing @"x-Foo"))
    assertIsNothing (testXNameUpperSymbol (symbolSing @"X-Foo"))
    assertIsNothing (testXNameUpperSymbol (symbolSing @"x-a"))
    assertIsNothing (testXNameUpperSymbol (symbolSing @"X-a"))
    assertIsNothing (testXNameUpperSymbol (symbolSing @"x-A"))
    --
    assertIsNothing (testXNameUpperSymbol (symbolSing @""))
    assertIsNothing (testXNameUpperSymbol (symbolSing @"a"))
    assertIsNothing (testXNameUpperSymbol (symbolSing @"A"))
    assertIsNothing (testXNameUpperSymbol (symbolSing @"foo"))
    assertIsNothing (testXNameUpperSymbol (symbolSing @"Foo"))
    assertIsNothing (testXNameUpperSymbol (symbolSing @"FOO"))
    assertIsNothing (testXNameUpperSymbol (symbolSing @"x"))
    assertIsNothing (testXNameUpperSymbol (symbolSing @"X"))
    assertIsNothing (testXNameUpperSymbol (symbolSing @"x-"))
    assertIsNothing (testXNameUpperSymbol (symbolSing @"X-"))
