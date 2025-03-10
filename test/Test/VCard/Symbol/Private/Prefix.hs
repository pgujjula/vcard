-- SPDX-FileCopyrightText: Copyright Preetham Gujjula
-- SPDX-License-Identifier: BSD-3-Clause

module Test.VCard.Symbol.Private.Prefix (tests) where

import Data.Char (toLower)
import Data.Dynamic (Dynamic, toDyn)
import Data.List (isPrefixOf)
import Data.Proxy (Proxy (..))
import Data.Singletons (fromSing)
import Data.Type.Equality ((:~:) (Refl))
import GHC.TypeLits (SomeSymbol (..), someSymbolVal)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (Assertion, testCase, (@?=))
import VCard.Symbol.Private.Compat (symbolSing)
import VCard.Symbol.Private.Prefix
  ( IsPrefixOf,
    IsPrefixOfInsensitive,
    sIsPrefixOf,
    sIsPrefixOfInsensitive,
  )

{-# ANN module ("HLint: ignore Use camelCase" :: String) #-}

tests :: TestTree
tests =
  testGroup
    "Prefix"
    [ test_IsPrefixOf,
      test_IsPrefixOfInsensitive,
      test_sIsPrefixOf,
      test_sIsPrefixOfInsensitive
    ]

test_IsPrefixOf :: TestTree
test_IsPrefixOf =
  testCase "IsPrefixOf" (seq cases_IsPrefixOf (pure ()))

cases_IsPrefixOf :: [Dynamic]
cases_IsPrefixOf =
  [ --
    -- Regular tests
    --
    toDyn (Refl :: IsPrefixOf "" "" :~: 'True),
    toDyn (Refl :: IsPrefixOf "" "a" :~: 'True),
    toDyn (Refl :: IsPrefixOf "" "abc" :~: 'True),
    toDyn (Refl :: IsPrefixOf "" "abcdef" :~: 'True),
    --
    toDyn (Refl :: IsPrefixOf "a" "" :~: 'False),
    toDyn (Refl :: IsPrefixOf "a" "a" :~: 'True),
    toDyn (Refl :: IsPrefixOf "a" "abc" :~: 'True),
    toDyn (Refl :: IsPrefixOf "a" "abcdef" :~: 'True),
    --
    toDyn (Refl :: IsPrefixOf "abc" "" :~: 'False),
    toDyn (Refl :: IsPrefixOf "abc" "a" :~: 'False),
    toDyn (Refl :: IsPrefixOf "abc" "abc" :~: 'True),
    toDyn (Refl :: IsPrefixOf "abc" "abcdef" :~: 'True),
    --
    toDyn (Refl :: IsPrefixOf "abcdef" "" :~: 'False),
    toDyn (Refl :: IsPrefixOf "abcdef" "a" :~: 'False),
    toDyn (Refl :: IsPrefixOf "abcdef" "abc" :~: 'False),
    toDyn (Refl :: IsPrefixOf "abcdef" "abcdef" :~: 'True),
    --
    -- Insensitive tests
    --
    toDyn (Refl :: IsPrefixOf "a" "" :~: 'False),
    toDyn (Refl :: IsPrefixOf "a" "A" :~: 'False),
    toDyn (Refl :: IsPrefixOf "a" "ABC" :~: 'False),
    toDyn (Refl :: IsPrefixOf "a" "ABCDEF" :~: 'False),
    --
    toDyn (Refl :: IsPrefixOf "abc" "" :~: 'False),
    toDyn (Refl :: IsPrefixOf "abc" "A" :~: 'False),
    toDyn (Refl :: IsPrefixOf "abc" "ABC" :~: 'False),
    toDyn (Refl :: IsPrefixOf "abc" "ABCDEF" :~: 'False)
  ]

test_sIsPrefixOf :: TestTree
test_sIsPrefixOf =
  testCase "sIsPrefixOf" $ do
    let isPrefixOfViaSingleton :: String -> String -> Bool
        isPrefixOfViaSingleton s t =
          case (someSymbolVal s, someSymbolVal t) of
            (SomeSymbol (Proxy :: Proxy s), SomeSymbol (Proxy :: Proxy t)) ->
              let ss = symbolSing @s
                  st = symbolSing @t
               in fromSing (sIsPrefixOf ss st)

        assertCase :: (String, String) -> Assertion
        assertCase (s, t) = isPrefixOfViaSingleton s t @?= isPrefixOf s t

    assertCase ("", "")
    assertCase ("", "a")
    assertCase ("", "abc")
    assertCase ("", "abcdef")
    --
    assertCase ("a", "")
    assertCase ("a", "a")
    assertCase ("a", "abc")
    assertCase ("a", "abcdef")
    --
    assertCase ("abc", "")
    assertCase ("abc", "a")
    assertCase ("abc", "abc")
    assertCase ("abc", "abcdef")
    --
    assertCase ("abcdef", "")
    assertCase ("abcdef", "a")
    assertCase ("abcdef", "abc")
    assertCase ("abcdef", "abcdef")
    --
    -- Insensitive tests
    --
    assertCase ("a", "")
    assertCase ("a", "A")
    assertCase ("a", "ABC")
    assertCase ("a", "ABCDEF")
    --
    assertCase ("abc", "")
    assertCase ("abc", "A")
    assertCase ("abc", "ABC")
    assertCase ("abc", "ABCDEF")

test_IsPrefixOfInsensitive :: TestTree
test_IsPrefixOfInsensitive =
  testCase
    "IsPrefixOfInsensitive"
    (seq cases_IsPrefixOfInsensitive (pure ()))

cases_IsPrefixOfInsensitive :: [Dynamic]
cases_IsPrefixOfInsensitive =
  [ --
    -- Regular tests
    --
    toDyn (Refl :: IsPrefixOf "" "" :~: 'True),
    toDyn (Refl :: IsPrefixOf "" "a" :~: 'True),
    toDyn (Refl :: IsPrefixOf "" "abc" :~: 'True),
    toDyn (Refl :: IsPrefixOf "" "abcdef" :~: 'True),
    --
    toDyn (Refl :: IsPrefixOf "a" "" :~: 'False),
    toDyn (Refl :: IsPrefixOf "a" "a" :~: 'True),
    toDyn (Refl :: IsPrefixOf "a" "abc" :~: 'True),
    toDyn (Refl :: IsPrefixOf "a" "abcdef" :~: 'True),
    --
    toDyn (Refl :: IsPrefixOf "abc" "" :~: 'False),
    toDyn (Refl :: IsPrefixOf "abc" "a" :~: 'False),
    toDyn (Refl :: IsPrefixOf "abc" "abc" :~: 'True),
    toDyn (Refl :: IsPrefixOf "abc" "abcdef" :~: 'True),
    --
    toDyn (Refl :: IsPrefixOf "abcdef" "" :~: 'False),
    toDyn (Refl :: IsPrefixOf "abcdef" "a" :~: 'False),
    toDyn (Refl :: IsPrefixOf "abcdef" "abc" :~: 'False),
    toDyn (Refl :: IsPrefixOf "abcdef" "abcdef" :~: 'True),
    --
    -- Insensitive tests
    --
    toDyn (Refl :: IsPrefixOfInsensitive "a" "" :~: 'False),
    toDyn (Refl :: IsPrefixOfInsensitive "a" "A" :~: 'True),
    toDyn (Refl :: IsPrefixOfInsensitive "a" "ABC" :~: 'True),
    toDyn (Refl :: IsPrefixOfInsensitive "a" "ABCDEF" :~: 'True),
    --
    toDyn (Refl :: IsPrefixOfInsensitive "abc" "" :~: 'False),
    toDyn (Refl :: IsPrefixOfInsensitive "abc" "A" :~: 'False),
    toDyn (Refl :: IsPrefixOfInsensitive "abc" "ABC" :~: 'True),
    toDyn (Refl :: IsPrefixOfInsensitive "abc" "ABCDEF" :~: 'True)
  ]

test_sIsPrefixOfInsensitive :: TestTree
test_sIsPrefixOfInsensitive =
  testCase "sIsPrefixInsensitiveOf" $ do
    let isPrefixOfInsensitiveViaSingleton :: String -> String -> Bool
        isPrefixOfInsensitiveViaSingleton s t =
          case (someSymbolVal s, someSymbolVal t) of
            (SomeSymbol (Proxy :: Proxy s), SomeSymbol (Proxy :: Proxy t)) ->
              let ss = symbolSing @s
                  st = symbolSing @t
               in fromSing (sIsPrefixOfInsensitive ss st)

        isPrefixOfInsensitive :: String -> String -> Bool
        isPrefixOfInsensitive s t = map toLower s `isPrefixOf` map toLower t

        assertCase :: (String, String) -> Assertion
        assertCase (s, t) =
          isPrefixOfInsensitiveViaSingleton s t @?= isPrefixOfInsensitive s t

    assertCase ("", "")
    assertCase ("", "a")
    assertCase ("", "abc")
    assertCase ("", "abcdef")
    --
    assertCase ("a", "")
    assertCase ("a", "a")
    assertCase ("a", "abc")
    assertCase ("a", "abcdef")
    --
    assertCase ("abc", "")
    assertCase ("abc", "a")
    assertCase ("abc", "abc")
    assertCase ("abc", "abcdef")
    --
    assertCase ("abcdef", "")
    assertCase ("abcdef", "a")
    assertCase ("abcdef", "abc")
    assertCase ("abcdef", "abcdef")
    --
    -- Insensitive tests
    --
    assertCase ("a", "")
    assertCase ("a", "A")
    assertCase ("a", "ABC")
    assertCase ("a", "ABCDEF")
    --
    assertCase ("abc", "")
    assertCase ("abc", "A")
    assertCase ("abc", "ABC")
    assertCase ("abc", "ABCDEF")
