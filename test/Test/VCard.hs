-- SPDX-FileCopyrightText: Copyright Preetham Gujjula
-- SPDX-License-Identifier: BSD-3-Clause

module Test.VCard (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))
import VCard (foo)

tests :: TestTree
tests =
  testGroup
    "VCard"
    [ fooTests
    ]

fooTests :: TestTree
fooTests =
  testGroup
    "foo"
    [ testCase "1" $ foo 1 @?= 2,
      testCase "2" $ foo 2 @?= 3,
      testCase "3" $ foo 3 @?= 4,
      testCase "4" $ foo 4 @?= 5
    ]
