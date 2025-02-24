-- SPDX-FileCopyrightText: Copyright Preetham Gujjula
-- SPDX-License-Identifier: BSD-3-Clause
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Test.VCard.Natural.Private.Compat (tests) where

import GHC.TypeLits (natVal)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))
import VCard.Natural.Private.Compat (natSing)

tests :: TestTree
tests =
  testGroup
    "Compat"
    [ test_natSing
    ]

test_natSing :: TestTree
test_natSing =
  testCase "natSing" $ do
    natVal (natSing @0) @?= 0
    natVal (natSing @1) @?= 1
    natVal (natSing @5) @?= 5
    natVal (natSing @123) @?= 123
    natVal (natSing @1000000000000000000000000) @?= 1000000000000000000000000
