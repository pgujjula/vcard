-- SPDX-FileCopyrightText: Copyright Preetham Gujjula
-- SPDX-License-Identifier: BSD-3-Clause
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}

module Test.VCard.Symbol.Private (tests) where

import GHC.TypeLits (charVal, symbolVal)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))
import VCard.Symbol.Private (charSing, symbolSing)

tests :: TestTree
tests =
  testGroup
    "Symbol"
    [ test_Symbol_charSing,
      test_Symbol_symbolSing
    ]

test_Symbol_charSing :: TestTree
test_Symbol_charSing =
  testCase "charSing" $ do
    charVal (charSing @'a') @?= 'a'
    charVal (charSing @'P') @?= 'P'
    charVal (charSing @'.') @?= '.'

test_Symbol_symbolSing :: TestTree
test_Symbol_symbolSing =
  testCase "symbolSing" $ do
    symbolVal (symbolSing @"") @?= ""
    symbolVal (symbolSing @"a") @?= "a"
    symbolVal (symbolSing @"Foo") @?= "Foo"
    symbolVal (symbolSing @"The quick brown fox") @?= "The quick brown fox"
