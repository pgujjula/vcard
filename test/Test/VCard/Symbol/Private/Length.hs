-- SPDX-FileCopyrightText: Copyright Preetham Gujjula
-- SPDX-License-Identifier: BSD-3-Clause
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module Test.VCard.Symbol.Private.Length (tests) where

import Data.Dynamic (Dynamic, toDyn)
import Data.Type.Equality ((:~:) (Refl))
import Test.Tasty (TestTree)
import Test.Tasty.HUnit (testCase)
import VCard.Symbol.Private.Length (Length)

tests :: TestTree
tests = testCase "Length" (seq test_Types (pure ()))

test_Types :: [Dynamic]
test_Types =
  [ toDyn (Refl :: Length "" :~: 0),
    toDyn (Refl :: Length "a" :~: 1),
    toDyn (Refl :: Length "abc" :~: 3),
    toDyn (Refl :: Length "Foo Bar" :~: 7)
  ]
