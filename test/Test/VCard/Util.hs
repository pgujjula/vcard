-- SPDX-FileCopyrightText: Copyright Preetham Gujjula
-- SPDX-License-Identifier: BSD-3-Clause

module Test.VCard.Util (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.VCard.Util.Natural qualified
import Test.VCard.Util.Symbol qualified

tests :: TestTree
tests =
  testGroup
    "Util"
    [ Test.VCard.Util.Natural.tests,
      Test.VCard.Util.Symbol.tests
    ]
