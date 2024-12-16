-- SPDX-FileCopyrightText: Copyright Preetham Gujjula
-- SPDX-License-Identifier: BSD-3-Clause

module Test.VCard (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))

tests :: TestTree
tests =
  testGroup
    "VCard"
    [ serializeTests
    ]

serializeTests :: TestTree
serializeTests =
  testGroup
    "serialize"
    [ testCase "empty serialization" $
        (1 :: Int) @?= 1
    ]
