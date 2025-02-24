-- SPDX-FileCopyrightText: Copyright Preetham Gujjula
-- SPDX-License-Identifier: BSD-3-Clause
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module Test.VCard.Symbol.Private.List (tests) where

import Data.Dynamic (Dynamic, toDyn)
import Data.Singletons (fromSing)
import Data.Type.Equality ((:~:) (Refl))
import GHC.TypeLits (symbolVal)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (Assertion, testCase, (@?=))
import VCard.Symbol.Private (SSymbol, symbolSing, withKnownSymbol)
import VCard.Symbol.Private.List (ToList, sToList)

tests :: TestTree
tests =
  testGroup
    "List"
    [ test_ToList,
      test_sToList
    ]

test_ToList :: TestTree
test_ToList = testCase "ToList" (seq refls (pure ()))
  where
    refls :: [Dynamic]
    refls =
      [ toDyn (Refl :: ToList "" :~: '[]),
        toDyn (Refl :: ToList "a" :~: '[ 'a']),
        toDyn
          (Refl :: ToList "Foo Bar" :~: '[ 'F', 'o', 'o', ' ', 'B', 'a', 'r'])
      ]

test_sToList :: TestTree
test_sToList =
  testCase "sToList" $ do
    let testSing :: SSymbol s -> Assertion
        testSing ss =
          fromSing (sToList ss) @?= withKnownSymbol ss (symbolVal ss)
    testSing (symbolSing @"")
    testSing (symbolSing @"a")
    testSing (symbolSing @"ab")
    testSing (symbolSing @"abc")
    testSing (symbolSing @"Foo Bar")
