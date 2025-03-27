-- SPDX-FileCopyrightText: Copyright Preetham Gujjula
-- SPDX-License-Identifier: BSD-3-Clause

module Test.VCard.Util.Symbol.Private.Slice (tests) where

import Data.Dynamic (Dynamic, toDyn)
import Data.Maybe (isJust)
import Data.Type.Equality ((:~:) (Refl))
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (Assertion, assertBool, testCase)
import VCard.Natural.Private (SNat, natSing)
import VCard.Util.Symbol.Private.Compat
  ( SSymbol,
    symbolSing,
    testSSymbolEquality,
  )
import VCard.Util.Symbol.Private.Slice (Drop, Take, sDrop, sTake)

tests :: TestTree
tests =
  testGroup
    "Slice"
    [ test_Take,
      test_sTake,
      test_Drop,
      test_sDrop
    ]

test_Take :: TestTree
test_Take = testCase "Take" (seq refls (pure ()))
  where
    refls :: [Dynamic]
    refls =
      [ toDyn (Refl :: Take 0 "" :~: ""),
        toDyn (Refl :: Take 1 "" :~: ""),
        toDyn (Refl :: Take 2 "" :~: ""),
        --
        toDyn (Refl :: Take 0 "a" :~: ""),
        toDyn (Refl :: Take 1 "a" :~: "a"),
        toDyn (Refl :: Take 2 "a" :~: "a"),
        toDyn (Refl :: Take 3 "a" :~: "a"),
        --
        toDyn (Refl :: Take 0 "abc" :~: ""),
        toDyn (Refl :: Take 1 "abc" :~: "a"),
        toDyn (Refl :: Take 2 "abc" :~: "ab"),
        toDyn (Refl :: Take 3 "abc" :~: "abc")
      ]

test_sTake :: TestTree
test_sTake =
  testCase "sTake" $ do
    let caseWith :: SNat n -> SSymbol a -> SSymbol b -> Assertion
        caseWith sn sa = assertEqualSSymbol (sTake sn sa)
    caseWith (natSing @0) (symbolSing @"") (symbolSing @"")
    caseWith (natSing @1) (symbolSing @"") (symbolSing @"")
    caseWith (natSing @2) (symbolSing @"") (symbolSing @"")
    --
    caseWith (natSing @0) (symbolSing @"a") (symbolSing @"")
    caseWith (natSing @1) (symbolSing @"a") (symbolSing @"a")
    caseWith (natSing @2) (symbolSing @"a") (symbolSing @"a")
    caseWith (natSing @3) (symbolSing @"a") (symbolSing @"a")
    --
    caseWith (natSing @0) (symbolSing @"abc") (symbolSing @"")
    caseWith (natSing @1) (symbolSing @"abc") (symbolSing @"a")
    caseWith (natSing @2) (symbolSing @"abc") (symbolSing @"ab")
    caseWith (natSing @3) (symbolSing @"abc") (symbolSing @"abc")

test_Drop :: TestTree
test_Drop = testCase "Drop" (seq refls (pure ()))
  where
    refls :: [Dynamic]
    refls =
      [ toDyn (Refl :: Drop 0 "" :~: ""),
        toDyn (Refl :: Drop 1 "" :~: ""),
        toDyn (Refl :: Drop 2 "" :~: ""),
        --
        toDyn (Refl :: Drop 0 "a" :~: "a"),
        toDyn (Refl :: Drop 1 "a" :~: ""),
        toDyn (Refl :: Drop 2 "a" :~: ""),
        toDyn (Refl :: Drop 3 "a" :~: ""),
        --
        toDyn (Refl :: Drop 0 "abc" :~: "abc"),
        toDyn (Refl :: Drop 1 "abc" :~: "bc"),
        toDyn (Refl :: Drop 2 "abc" :~: "c"),
        toDyn (Refl :: Drop 3 "abc" :~: "")
      ]

test_sDrop :: TestTree
test_sDrop =
  testCase "sDrop" $ do
    let caseWith :: SNat n -> SSymbol a -> SSymbol b -> Assertion
        caseWith sn sa = assertEqualSSymbol (sDrop sn sa)
    caseWith (natSing @0) (symbolSing @"") (symbolSing @"")
    caseWith (natSing @1) (symbolSing @"") (symbolSing @"")
    caseWith (natSing @2) (symbolSing @"") (symbolSing @"")
    --
    caseWith (natSing @0) (symbolSing @"a") (symbolSing @"a")
    caseWith (natSing @1) (symbolSing @"a") (symbolSing @"")
    caseWith (natSing @2) (symbolSing @"a") (symbolSing @"")
    caseWith (natSing @3) (symbolSing @"a") (symbolSing @"")
    --
    caseWith (natSing @0) (symbolSing @"abc") (symbolSing @"abc")
    caseWith (natSing @1) (symbolSing @"abc") (symbolSing @"bc")
    caseWith (natSing @2) (symbolSing @"abc") (symbolSing @"c")
    caseWith (natSing @3) (symbolSing @"abc") (symbolSing @"")

-- Utilities
assertEqualSSymbol :: SSymbol a -> SSymbol b -> Assertion
assertEqualSSymbol sa sb =
  assertBool "expected equal SSymbols" $
    isJust (testSSymbolEquality sa sb)
