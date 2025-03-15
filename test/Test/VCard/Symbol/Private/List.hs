-- SPDX-FileCopyrightText: Copyright Preetham Gujjula
-- SPDX-License-Identifier: BSD-3-Clause

module Test.VCard.Symbol.Private.List (tests) where

import Data.Dynamic (Dynamic, toDyn)
import Data.List.Singletons (SList)
import Data.Singletons (fromSing, withSomeSing)
import Data.Type.Equality ((:~:) (Refl))
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (Assertion, testCase, (@?=))
import VCard.Symbol.Private (SSymbol, fromSSymbol, symbolSing)
import VCard.Symbol.Private.List (FromList, ToList, sFromList, sToList)

tests :: TestTree
tests =
  testGroup
    "List"
    [ test_ToList,
      test_sToList,
      test_FromList,
      test_sFromList
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
        testSing ss = fromSing (sToList ss) @?= fromSSymbol ss
    testSing (symbolSing @"")
    testSing (symbolSing @"a")
    testSing (symbolSing @"ab")
    testSing (symbolSing @"abc")
    testSing (symbolSing @"Foo Bar")

test_FromList :: TestTree
test_FromList = testCase "FromList" (seq refls (pure ()))
  where
    refls :: [Dynamic]
    refls =
      [ toDyn (Refl :: FromList '[] :~: ""),
        toDyn (Refl :: FromList '[ 'a'] :~: "a"),
        toDyn
          (Refl :: FromList '[ 'F', 'o', 'o', ' ', 'B', 'a', 'r'] :~: "Foo Bar")
      ]

test_sFromList :: TestTree
test_sFromList =
  testCase "sFromList" $ do
    let testSing :: [Char] -> Assertion
        testSing xs =
          withSomeSing xs $ \(schars :: SList (xs :: [Char])) ->
            fromSSymbol (sFromList schars) @?= xs
    testSing ""
    testSing "a"
    testSing "ab"
    testSing "abc"
    testSing "Foo Bar"
