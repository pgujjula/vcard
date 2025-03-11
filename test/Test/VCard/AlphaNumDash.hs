-- SPDX-FileCopyrightText: Copyright Preetham Gujjula
-- SPDX-License-Identifier: BSD-3-Clause

module Test.VCard.AlphaDigitDash (tests) where

import Data.Maybe (isJust, isNothing)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (Assertion, assertBool, testCase)
import Test.VCard.AlphaDigitDash.TypeTests
  ( test_AlphaDigitDashLowerSymbol,
    test_AlphaDigitDashSymbol,
    test_AlphaDigitDashUpperSymbol,
  )
import VCard.AlphaDigitDash
  ( testAlphaDigitDashLowerSymbol,
    testAlphaDigitDashSymbol,
    testAlphaDigitDashUpperSymbol,
  )
import VCard.Symbol.Private (symbolSing)

tests :: TestTree
tests =
  testGroup
    "AlphaDigitDash"
    [ test_AlphaDigitDashSymbol,
      test_testAlphaDigitDashSymbol,
      test_AlphaDigitDashLowerSymbol,
      test_testAlphaDigitDashLowerSymbol,
      test_AlphaDigitDashUpperSymbol,
      test_testAlphaDigitDashUpperSymbol
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
