-- SPDX-FileCopyrightText: Copyright Preetham Gujjula
-- SPDX-License-Identifier: BSD-3-Clause
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Test.VCard.Symbol.Private.Compat (tests) where

import Data.Maybe (isJust, isNothing)
import Data.Maybe.Singletons (SMaybe (SJust, SNothing))
import Data.Tuple.Singletons (STuple2 (..))
import GHC.TypeLits (charVal, symbolVal)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (Assertion, assertBool, testCase, (@?=))
import VCard.Symbol.Private.Compat
  ( SChar,
    SSymbol,
    charSing,
    sUnconsSymbol,
    symbolSing,
    testSCharEquality,
    testSSymbolEquality,
  )

tests :: TestTree
tests =
  testGroup
    "Compat"
    [ test_charSing,
      test_symbolSing,
      test_testSCharEquality,
      test_testSSymbolEquality,
      test_sUnconsSymbol
    ]

test_charSing :: TestTree
test_charSing =
  testCase "charSing" $ do
    charVal (charSing @'a') @?= 'a'
    charVal (charSing @'P') @?= 'P'
    charVal (charSing @'.') @?= '.'

test_symbolSing :: TestTree
test_symbolSing =
  testCase "symbolSing" $ do
    symbolVal (symbolSing @"") @?= ""
    symbolVal (symbolSing @"a") @?= "a"
    symbolVal (symbolSing @"Foo") @?= "Foo"
    symbolVal (symbolSing @"The quick brown fox") @?= "The quick brown fox"

test_testSCharEquality :: TestTree
test_testSCharEquality =
  testCase "testSCharEquality" $ do
    assertEqualSChar (charSing @'a') (charSing @'a')
    assertEqualSChar (charSing @'\t') (charSing @'\t')
    assertUnequalSChar (charSing @' ') (charSing @'\t')
    assertUnequalSChar (charSing @'.') (charSing @',')

test_testSSymbolEquality :: TestTree
test_testSSymbolEquality =
  testCase "testSSymbolEquality" $ do
    assertEqualSSymbol (symbolSing @"") (symbolSing @"")
    assertEqualSSymbol (symbolSing @"a") (symbolSing @"a")
    assertEqualSSymbol (symbolSing @"Foo") (symbolSing @"Foo")
    assertEqualSSymbol
      (symbolSing @"The quick brown fox")
      (symbolSing @"The quick brown fox")

    assertUnequalSSymbol (symbolSing @"") (symbolSing @"a")
    assertUnequalSSymbol (symbolSing @"abc") (symbolSing @"ab c")
    assertUnequalSSymbol (symbolSing @"Foo") (symbolSing @"foo")
    assertUnequalSSymbol
      (symbolSing @"The quick brown fox\n")
      (symbolSing @"The quick brown fox\r\n")

test_sUnconsSymbol :: TestTree
test_sUnconsSymbol =
  testCase "sUnconsSymbol" $ do
    case sUnconsSymbol (symbolSing @"") of
      SNothing -> pure ()
    case sUnconsSymbol (symbolSing @"a") of
      SJust (STuple2 sc ss) -> do
        assertEqualSChar sc (charSing @'a')
        assertEqualSSymbol ss (symbolSing @"")
    case sUnconsSymbol (symbolSing @"abc") of
      SJust (STuple2 sc ss) -> do
        assertEqualSChar sc (charSing @'a')
        assertEqualSSymbol ss (symbolSing @"bc")

-- Utilities
assertEqualSChar :: SChar a -> SChar b -> Assertion
assertEqualSChar sa sb =
  assertBool "expected equal SChars" $
    isJust (testSCharEquality sa sb)

assertUnequalSChar :: SChar a -> SChar b -> Assertion
assertUnequalSChar sa sb =
  assertBool "expected different SChars" $
    isNothing (testSCharEquality sa sb)

assertEqualSSymbol :: SSymbol a -> SSymbol b -> Assertion
assertEqualSSymbol sa sb =
  assertBool "expected equal SSymbols" $
    isJust (testSSymbolEquality sa sb)

assertUnequalSSymbol :: SSymbol a -> SSymbol b -> Assertion
assertUnequalSSymbol sa sb =
  assertBool "expected different SSymbols" $
    isNothing (testSSymbolEquality sa sb)
