-- SPDX-FileCopyrightText: Copyright Preetham Gujjula
-- SPDX-License-Identifier: BSD-3-Clause
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Test.VCard.Symbol (tests) where

import Data.Maybe (isJust)
import GHC.TypeLits.Singletons (SSymbol)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (Assertion, assertBool, testCase)
import VCard.Symbol (sToLower, sToUpper)
import VCard.Symbol.Private (symbolSing, testSSymbolEquality)

tests :: TestTree
tests =
  testGroup
    "Symbol"
    [ test_Symbol_sToLower,
      test_Symbol_sToUpper
    ]

test_Symbol_sToLower :: TestTree
test_Symbol_sToLower =
  testCase "sToLower" $ do
    let check :: SSymbol a -> SSymbol b -> Assertion
        check sa sb =
          assertBool "expected Just" $
            isJust (testSSymbolEquality (sToLower sa) sb)
    check (symbolSing @"") (symbolSing @"")
    check (symbolSing @"A") (symbolSing @"a")
    check (symbolSing @"foo BaR") (symbolSing @"foo bar")
    check
      (symbolSing @"ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz")
      (symbolSing @"abcdefghijklmnopqrstuvwxyzabcdefghijklmnopqrstuvwxyz")
    check
      (symbolSing @"0123456789!@#$%^&*()")
      (symbolSing @"0123456789!@#$%^&*()")

test_Symbol_sToUpper :: TestTree
test_Symbol_sToUpper =
  testCase "sToUpper" $ do
    let check :: SSymbol a -> SSymbol b -> Assertion
        check sa sb =
          assertBool "expected Just" $
            isJust (testSSymbolEquality (sToUpper sa) sb)
    check (symbolSing @"") (symbolSing @"")
    check (symbolSing @"a") (symbolSing @"A")
    check (symbolSing @"foo BaR") (symbolSing @"FOO BAR")
    check
      (symbolSing @"ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz")
      (symbolSing @"ABCDEFGHIJKLMNOPQRSTUVWXYZABCDEFGHIJKLMNOPQRSTUVWXYZ")
    check
      (symbolSing @"0123456789!@#$%^&*()")
      (symbolSing @"0123456789!@#$%^&*()")
