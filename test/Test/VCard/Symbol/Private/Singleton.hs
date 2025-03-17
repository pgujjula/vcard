-- SPDX-FileCopyrightText: Copyright Preetham Gujjula
-- SPDX-License-Identifier: BSD-3-Clause

module Test.VCard.Symbol.Private.Singleton (tests) where

import Data.Maybe (isJust)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (Assertion, assertBool, testCase)
import VCard.Symbol.Private.Compat
  ( SSymbol,
    charSing,
    symbolSing,
    testSSymbolEquality,
  )
import VCard.Symbol.Private.Singleton (sSingletonSymbol)

tests :: TestTree
tests =
  testGroup
    "Slice"
    [ test_sSingleton
    ]

test_sSingleton :: TestTree
test_sSingleton = testCase "sSingletonSymbol" $ do
  assertEqualSSymbol (sSingletonSymbol (charSing @'a')) (symbolSing @"a")
  assertEqualSSymbol (sSingletonSymbol (charSing @'\n')) (symbolSing @"\n")
  assertEqualSSymbol (sSingletonSymbol (charSing @'\x00')) (symbolSing @"\x00")

-- Utilities
assertEqualSSymbol :: SSymbol a -> SSymbol b -> Assertion
assertEqualSSymbol sa sb =
  assertBool "expected equal SSymbols" $
    isJust (testSSymbolEquality sa sb)
