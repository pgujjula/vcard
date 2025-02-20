-- SPDX-FileCopyrightText: Copyright Preetham Gujjula
-- SPDX-License-Identifier: BSD-3-Clause
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Test.VCard.Symbol.Private.Case (tests) where

import Control.Monad (forM_)
import Data.Char (toLower, toUpper)
import Data.Proxy (Proxy (..))
import GHC.TypeLits
  ( SomeChar (..),
    SomeSymbol (..),
    charVal,
    someCharVal,
    someSymbolVal,
    symbolVal,
  )
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))
import VCard.Symbol.Private.Case
  ( ToLower,
    ToLowerChar,
    ToUpper,
    ToUpperChar,
    sToLower,
    sToLowerChar,
    sToUpper,
    sToUpperChar,
  )
import VCard.Symbol.Private.Compat
  ( SChar,
    SSymbol,
    charSing,
    symbolSing,
    withKnownChar,
    withKnownSymbol,
  )

tests :: TestTree
tests =
  testGroup
    "Case"
    [ test_sToLower,
      test_sToLowerChar,
      test_sToUpper,
      test_sToUpperChar
    ]

test_sToLower :: TestTree
test_sToLower =
  testCase "sToLower" $ do
    let toLowerViaSingleton :: String -> String
        toLowerViaSingleton s =
          case someSymbolVal s of
            SomeSymbol (Proxy :: Proxy s) ->
              let ss :: SSymbol s
                  ss = symbolSing

                  ss' :: SSymbol (ToLower s)
                  ss' = sToLower ss
               in withKnownSymbol ss' $ symbolVal ss'
    forM_ ["", "abc", "DEF", "Foo", asciiChars] $ \s ->
      toLowerViaSingleton s @?= map toLower s

test_sToLowerChar :: TestTree
test_sToLowerChar =
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

test_sToUpper :: TestTree
test_sToUpper =
  testCase "sToUpper" $ do
    let toUpperViaSingleton :: String -> String
        toUpperViaSingleton s =
          case someSymbolVal s of
            SomeSymbol (Proxy :: Proxy s) ->
              let ss :: SSymbol s
                  ss = symbolSing

                  ss' :: SSymbol (ToUpper s)
                  ss' = sToUpper ss
               in withKnownSymbol ss' $ symbolVal ss'
    forM_ ["", "abc", "DEF", "Foo", asciiChars] $ \s ->
      toUpperViaSingleton s @?= map toUpper s

test_sToUpperChar :: TestTree
test_sToUpperChar =
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
