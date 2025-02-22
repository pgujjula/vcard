-- SPDX-FileCopyrightText: Copyright Preetham Gujjula
-- SPDX-License-Identifier: BSD-3-Clause
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module Test.VCard.Symbol.Private.List (tests) where

import Data.Dynamic (Dynamic, toDyn)
import Data.Type.Equality ((:~:) (Refl))
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase)
import VCard.Symbol.Private.List (ToList)

tests :: TestTree
tests =
  testGroup
    "List"
    [ testCase "ToList" (seq test_Types (pure ()))
    ]

test_Types :: [Dynamic]
test_Types =
  [ toDyn (Refl :: ToList "" :~: '[]),
    toDyn (Refl :: ToList "a" :~: '[ 'a']),
    toDyn (Refl :: ToList "Foo Bar" :~: '[ 'F', 'o', 'o', ' ', 'B', 'a', 'r'])
  ]
