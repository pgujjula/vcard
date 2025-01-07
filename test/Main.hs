-- SPDX-FileCopyrightText: Copyright Preetham Gujjula
-- SPDX-License-Identifier: BSD-3-Clause

import Test.Tasty (TestTree, defaultMain, testGroup)
import Test.VCard qualified (tests)

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests =
  testGroup
    ""
    [ Test.VCard.tests
    ]
