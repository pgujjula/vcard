-- SPDX-FileCopyrightText: Copyright Preetham Gujjula
-- SPDX-License-Identifier: BSD-3-Clause

module Test.VCard.Symbol.Private.Snoc (tests) where

import Data.Maybe (isJust)
import Data.Maybe.Singletons (SMaybe (SJust, SNothing))
import Data.Singletons.Decide (decideEquality)
import Data.Tuple.Singletons (STuple2 (..))
import GHC.TypeLits (Symbol)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (Assertion, assertBool, testCase)
import VCard.Symbol.Private.Compat
  ( SSymbol,
    charSing,
    symbolSing,
    testSSymbolEquality,
  )
import VCard.Symbol.Private.Snoc (sSnocSymbol, sUnsnocSymbol)

tests :: TestTree
tests =
  testGroup
    "Snoc"
    [ test_sSnocSymbol,
      test_sUnsnocSymbol
    ]

test_sSnocSymbol :: TestTree
test_sSnocSymbol = testCase "sSnocSymbol" $ do
  assertEqualSSymbol
    (sSnocSymbol (symbolSing @"") (charSing @'b'))
    (symbolSing @"b")
  assertEqualSSymbol
    (sSnocSymbol (symbolSing @"foo") (charSing @'b'))
    (symbolSing @"foob")

test_sUnsnocSymbol :: TestTree
test_sUnsnocSymbol = testCase "sUnsnocSymbol" $ do
  assertEqualUnsnoc (sUnsnocSymbol (symbolSing @"")) SNothing
  assertEqualUnsnoc
    (sUnsnocSymbol (symbolSing @"a"))
    (SJust (STuple2 (symbolSing @"") (charSing @'a')))
  assertEqualUnsnoc
    (sUnsnocSymbol (symbolSing @"ab"))
    (SJust (STuple2 (symbolSing @"a") (charSing @'b')))
  assertEqualUnsnoc
    (sUnsnocSymbol (symbolSing @"abc"))
    (SJust (STuple2 (symbolSing @"ab") (charSing @'c')))
  assertEqualUnsnoc
    (sUnsnocSymbol (symbolSing @"foobar"))
    (SJust (STuple2 (symbolSing @"fooba") (charSing @'r')))

-- Utilities
assertEqualSSymbol :: SSymbol a -> SSymbol b -> Assertion
assertEqualSSymbol sa sb =
  assertBool "expected equal SSymbols" $
    isJust (testSSymbolEquality sa sb)

assertEqualUnsnoc ::
  SMaybe (a :: Maybe (Symbol, Char)) -> SMaybe (b :: Maybe (Symbol, Char)) -> Assertion
assertEqualUnsnoc sa sb =
  assertBool "expected equal singletons" $
    isJust (decideEquality sa sb)
