-- SPDX-FileCopyrightText: Copyright Preetham Gujjula
-- SPDX-License-Identifier: BSD-3-Clause
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Test.VCard.Symbol.Private.Case (tests) where

import Control.Monad (forM_)
import Data.Char (toLower, toUpper)
import Data.Proxy (Proxy (..))
import GHC.TypeLits (SomeChar (..), charVal, someCharVal)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))
import VCard.Symbol.Private
  ( SChar,
    ToLowerChar,
    ToUpperChar,
    charSing,
    sToLowerChar,
    sToUpperChar,
    withKnownChar,
  )

tests :: TestTree
tests =
  testGroup
    "Case"
    [ test_sToLowerChar,
      test_sToUpperChar
    ]

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
