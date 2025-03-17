-- SPDX-FileCopyrightText: Copyright Preetham Gujjula
-- SPDX-License-Identifier: BSD-3-Clause
{-# LANGUAGE OverloadedStrings #-}

module Test.VCard.Symbol.Private.Parse (tests) where

import Test.Tasty (TestTree)
import Test.Tasty.HUnit (Assertion, assertBool, testCase)
import VCard.Parse (parse)
import VCard.Symbol.Private (SSymbol, fromSSymbol, symbolSing)

tests :: TestTree
tests = testCase "Parse" $ do
  assertEqualSSymbol (parse @(SSymbol "") "") (Just (symbolSing @""))
  assertEqualSSymbol (parse @(SSymbol "abc") "abc") (Just (symbolSing @"abc"))

  assertEqualSSymbol (parse @(SSymbol "") "abc") Nothing
  assertEqualSSymbol (parse @(SSymbol "abc") "") Nothing
  assertEqualSSymbol (parse @(SSymbol "abc") "ab") Nothing
  assertEqualSSymbol (parse @(SSymbol "abc") "abcd") Nothing

-- Utilities
assertEqualSSymbol :: Maybe (SSymbol a) -> Maybe (SSymbol b) -> Assertion
assertEqualSSymbol sa sb =
  assertBool "expected equal SSymbols" $
    (fromSSymbol <$> sa) == (fromSSymbol <$> sb)
