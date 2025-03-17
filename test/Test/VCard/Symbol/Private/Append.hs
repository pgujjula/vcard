-- SPDX-FileCopyrightText: Copyright Preetham Gujjula
-- SPDX-License-Identifier: BSD-3-Clause

module Test.VCard.Symbol.Private.Append (tests) where

import Data.Maybe (isJust)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (Assertion, assertBool, testCase)
import VCard.Symbol.Private.Append (sAppendSymbol)
import VCard.Symbol.Private.Compat (SSymbol, symbolSing, testSSymbolEquality)

tests :: TestTree
tests =
  testGroup
    "AppendSymbol"
    [ test_sAppendSymbol
    ]

test_sAppendSymbol :: TestTree
test_sAppendSymbol =
  testCase "sAppendSymbol" $ do
    assertEqualSSymbol
      (sAppendSymbol (symbolSing @"") (symbolSing @""))
      (symbolSing @"")

    assertEqualSSymbol
      (sAppendSymbol (symbolSing @"foo") (symbolSing @""))
      (symbolSing @"foo")

    assertEqualSSymbol
      (sAppendSymbol (symbolSing @"") (symbolSing @"bar"))
      (symbolSing @"bar")

    assertEqualSSymbol
      (sAppendSymbol (symbolSing @"foo") (symbolSing @"bar"))
      (symbolSing @"foobar")

-- Utilities
assertEqualSSymbol :: SSymbol a -> SSymbol b -> Assertion
assertEqualSSymbol sa sb =
  assertBool "expected equal SSymbols" $
    isJust (testSSymbolEquality sa sb)
