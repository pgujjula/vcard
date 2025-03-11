-- SPDX-FileCopyrightText: Copyright Preetham Gujjula
-- SPDX-License-Identifier: BSD-3-Clause

module Test.VCard.AlphaNumDash (tests) where

import Data.Maybe (isJust, isNothing)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (Assertion, assertBool, testCase)
import Test.VCard.AlphaNumDash.TypeTests
  ( test_AlphaNumDashLowerSymbol,
    test_AlphaNumDashSymbol,
    test_AlphaNumDashUpperSymbol,
  )
import VCard.AlphaNumDash
  ( testAlphaNumDashLowerSymbol,
    testAlphaNumDashSymbol,
    testAlphaNumDashUpperSymbol,
  )
import VCard.Symbol.Private (symbolSing)

tests :: TestTree
tests =
  testGroup
    "AlphaNumDash"
    [ test_AlphaNumDashSymbol,
      test_testAlphaNumDashSymbol,
      test_AlphaNumDashLowerSymbol,
      test_testAlphaNumDashLowerSymbol,
      test_AlphaNumDashUpperSymbol,
      test_testAlphaNumDashUpperSymbol
    ]

assertIsJust :: Maybe a -> Assertion
assertIsJust x = assertBool "expected Just" (isJust x)

assertIsNothing :: Maybe a -> Assertion
assertIsNothing x = assertBool "expected Nothing" (isNothing x)

test_testAlphaNumDashSymbol :: TestTree
test_testAlphaNumDashSymbol =
  testCase "testAlphaNumDashSymbol" $ do
    assertIsJust (testAlphaNumDashSymbol (symbolSing @"a"))
    assertIsJust (testAlphaNumDashSymbol (symbolSing @"foo-BAR-123"))
    assertIsJust
      ( testAlphaNumDashSymbol
          ( symbolSing
              @"abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789-"
          )
      )
    --
    assertIsNothing (testAlphaNumDashSymbol (symbolSing @""))
    assertIsNothing (testAlphaNumDashSymbol (symbolSing @"foo-BAR-!23"))

test_testAlphaNumDashLowerSymbol :: TestTree
test_testAlphaNumDashLowerSymbol =
  testCase "testAlphaNumDashLowerSymbol" $ do
    assertIsJust (testAlphaNumDashLowerSymbol (symbolSing @"a"))
    assertIsJust (testAlphaNumDashLowerSymbol (symbolSing @"foo-bar-123"))
    assertIsJust
      ( testAlphaNumDashLowerSymbol
          ( symbolSing
              @"abcdefghijklmnopqrstuvwxyz0123456789-"
          )
      )
    --
    assertIsNothing (testAlphaNumDashLowerSymbol (symbolSing @""))
    assertIsNothing (testAlphaNumDashLowerSymbol (symbolSing @"foo-bAr-123"))
    assertIsNothing (testAlphaNumDashLowerSymbol (symbolSing @"foo-bar-!23"))

test_testAlphaNumDashUpperSymbol :: TestTree
test_testAlphaNumDashUpperSymbol =
  testCase "testAlphaNumDashUpperSymbol" $ do
    assertIsJust (testAlphaNumDashUpperSymbol (symbolSing @"A"))
    assertIsJust (testAlphaNumDashUpperSymbol (symbolSing @"FOO-BAR-123"))
    assertIsJust
      ( testAlphaNumDashUpperSymbol
          ( symbolSing
              @"ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789-"
          )
      )
    --
    assertIsNothing (testAlphaNumDashUpperSymbol (symbolSing @""))
    assertIsNothing (testAlphaNumDashUpperSymbol (symbolSing @"FOO-bAR-123"))
    assertIsNothing (testAlphaNumDashUpperSymbol (symbolSing @"FOO-BAR-!23"))
