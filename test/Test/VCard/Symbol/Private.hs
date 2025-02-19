-- SPDX-FileCopyrightText: Copyright Preetham Gujjula
-- SPDX-License-Identifier: BSD-3-Clause
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Test.VCard.Symbol.Private (tests) where

import Control.Monad (forM_)
import Data.Char (toLower, toUpper)
import Data.Maybe (isJust, isNothing)
import Data.Proxy (Proxy (..))
import GHC.TypeLits (SomeChar (..), charVal, someCharVal, symbolVal)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (Assertion, assertBool, testCase, (@?=))
import VCard.Symbol.Private
  ( SChar,
    SSymbol,
    ToLowerChar,
    ToUpperChar,
    charSing,
    sToLowerChar,
    sToUpperChar,
    symbolSing,
    testSCharEquality,
    testSSymbolEquality,
    withKnownChar,
  )

tests :: TestTree
tests =
  testGroup
    "Symbol"
    [ test_Symbol_charSing,
      test_Symbol_symbolSing,
      test_Symbol_testSCharEquality,
      test_Symbol_testSSymbolEquality,
      test_Symbol_sToLowerChar,
      test_Symbol_sToUpperChar
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

test_Symbol_testSCharEquality :: TestTree
test_Symbol_testSCharEquality =
  testCase "testSCharEquality" $ do
    let testEqual :: SChar a -> SChar b -> Assertion
        testEqual sa sb =
          assertBool "expected equal SChars" $
            isJust (testSCharEquality sa sb)

        testUnequal :: SChar a -> SChar b -> Assertion
        testUnequal sa sb =
          assertBool "expected different SChars" $
            isNothing (testSCharEquality sa sb)

    testEqual (charSing @'a') (charSing @'a')
    testEqual (charSing @'\t') (charSing @'\t')
    testUnequal (charSing @' ') (charSing @'\t')
    testUnequal (charSing @'.') (charSing @',')

test_Symbol_testSSymbolEquality :: TestTree
test_Symbol_testSSymbolEquality =
  testCase "testSSymbolEquality" $ do
    let testEqual :: SSymbol a -> SSymbol b -> Assertion
        testEqual sa sb =
          assertBool "expected equal SSymbols" $
            isJust (testSSymbolEquality sa sb)

        testUnequal :: SSymbol a -> SSymbol b -> Assertion
        testUnequal sa sb =
          assertBool "expected different SSymbols" $
            isNothing (testSSymbolEquality sa sb)

    testEqual (symbolSing @"") (symbolSing @"")
    testEqual (symbolSing @"a") (symbolSing @"a")
    testEqual (symbolSing @"Foo") (symbolSing @"Foo")
    testEqual
      (symbolSing @"The quick brown fox")
      (symbolSing @"The quick brown fox")

    testUnequal (symbolSing @"") (symbolSing @"a")
    testUnequal (symbolSing @"abc") (symbolSing @"ab c")
    testUnequal (symbolSing @"Foo") (symbolSing @"foo")
    testUnequal
      (symbolSing @"The quick brown fox\n")
      (symbolSing @"The quick brown fox\r\n")

test_Symbol_sToLowerChar :: TestTree
test_Symbol_sToLowerChar =
  testCase "sToLowerChar" $ do
    let toLowerViaSingleton :: Char -> Char
        toLowerViaSingleton c =
          case someCharVal c of
            SomeChar (Proxy :: Proxy c) ->
              let sc :: SChar c
                  sc = charSing

                  sc' :: SChar (ToLowerChar c)
                  sc' = sToLowerChar sc
               in withKnownChar sc' $ charVal sc'
    forM_ asciiChars $ \c ->
      toLowerViaSingleton c @?= toLower c

test_Symbol_sToUpperChar :: TestTree
test_Symbol_sToUpperChar =
  testCase "sToUpperChar" $ do
    let toUpperViaSingleton :: Char -> Char
        toUpperViaSingleton c =
          case someCharVal c of
            SomeChar (Proxy :: Proxy c) ->
              let sc :: SChar c
                  sc = charSing

                  sc' :: SChar (ToUpperChar c)
                  sc' = sToUpperChar sc
               in withKnownChar sc' $ charVal sc'
    forM_ asciiChars $ \c ->
      toUpperViaSingleton c @?= toUpper c

-- Utilities
asciiChars :: [Char]
asciiChars = ['\0' .. '\127']
