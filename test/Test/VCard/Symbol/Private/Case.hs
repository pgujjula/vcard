-- SPDX-FileCopyrightText: Copyright Preetham Gujjula
-- SPDX-License-Identifier: BSD-3-Clause

module Test.VCard.Symbol.Private.Case (tests) where

import Control.Monad (forM_)
import Data.Char (toLower, toUpper)
import Data.Dynamic (Dynamic, toDyn)
import Data.Proxy (Proxy (..))
import Data.Type.Equality ((:~:) (Refl))
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
  -- We include test_Types here to avoid an unused-top-binds warning
  seq test_Types $
    testGroup
      "Case"
      [ test_sToLower,
        test_sToLowerChar,
        test_sToUpper,
        test_sToUpperChar
      ]

test_Types :: [Dynamic]
test_Types =
  concat
    [ test_ToLower,
      test_ToUpper
    ]

test_ToLower :: [Dynamic]
test_ToLower =
  [ toDyn (Refl :: (ToLower "" :~: "")),
    toDyn (Refl :: (ToLower "abc" :~: "abc")),
    toDyn (Refl :: (ToLower "DEF" :~: "def")),
    toDyn (Refl :: (ToLower "Foo" :~: "foo")),
    toDyn
      ( Refl ::
          ( ToLower "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ"
              :~: "abcdefghijklmnopqrstuvwxyzabcdefghijklmnopqrstuvwxyz"
          )
      ),
    toDyn
      ( Refl ::
          ( ToLower "1234567890!@#$%^&*()-_=+`~[{]}\\|;:'\",<.>/? \t\r\n"
              :~: "1234567890!@#$%^&*()-_=+`~[{]}\\|;:'\",<.>/? \t\r\n"
          )
      )
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

test_ToUpper :: [Dynamic]
test_ToUpper =
  [ toDyn (Refl :: (ToUpper "" :~: "")),
    toDyn (Refl :: (ToUpper "abc" :~: "ABC")),
    toDyn (Refl :: (ToUpper "DEF" :~: "DEF")),
    toDyn (Refl :: (ToUpper "Foo" :~: "FOO")),
    toDyn
      ( Refl ::
          ( ToUpper "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ"
              :~: "ABCDEFGHIJKLMNOPQRSTUVWXYZABCDEFGHIJKLMNOPQRSTUVWXYZ"
          )
      ),
    toDyn
      ( Refl ::
          ( ToUpper "1234567890!@#$%^&*()-_=+`~[{]}\\|;:'\",<.>/? \t\r\n"
              :~: "1234567890!@#$%^&*()-_=+`~[{]}\\|;:'\",<.>/? \t\r\n"
          )
      )
  ]

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
