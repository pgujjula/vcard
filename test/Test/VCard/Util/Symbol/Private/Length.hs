-- SPDX-FileCopyrightText: Copyright Preetham Gujjula
-- SPDX-License-Identifier: BSD-3-Clause

module Test.VCard.Util.Symbol.Private.Length (tests) where

import Data.Dynamic (Dynamic, toDyn)
import Data.Proxy (Proxy (..))
import Data.Type.Equality ((:~:) (Refl))
import GHC.TypeLits (SomeSymbol (..), natVal, someSymbolVal)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (Assertion, testCase, (@?=))
import VCard.Util.Natural.Private.Compat (withKnownNat)
import VCard.Util.Symbol.Private.Compat (symbolSing)
import VCard.Util.Symbol.Private.Length (Length, sLength)

{-# ANN module ("HLint: ignore Use camelCase" :: String) #-}

tests :: TestTree
tests =
  testGroup
    "Length"
    [ test_Length,
      test_sLength
    ]

test_Length :: TestTree
test_Length = testCase "Length" (seq cases_Length (pure ()))

cases_Length :: [Dynamic]
cases_Length =
  [ toDyn (Refl :: Length "" :~: 0),
    toDyn (Refl :: Length "a" :~: 1),
    toDyn (Refl :: Length "abc" :~: 3),
    toDyn (Refl :: Length "Foo Bar" :~: 7)
  ]

test_sLength :: TestTree
test_sLength =
  testCase "sLength" $ do
    let lengthViaSingleton :: String -> Int
        lengthViaSingleton xs =
          case someSymbolVal xs of
            SomeSymbol (Proxy :: Proxy s) ->
              let sl = sLength (symbolSing @s)
               in fromInteger (withKnownNat sl (natVal sl))
        checkLength :: String -> Assertion
        checkLength xs = lengthViaSingleton xs @?= length xs
    checkLength ""
    checkLength "a"
    checkLength "Foo Bar"
    checkLength (replicate 100 'z')
