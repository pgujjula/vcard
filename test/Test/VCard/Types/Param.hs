-- SPDX-FileCopyrightText: Copyright Preetham Gujjula
-- SPDX-License-Identifier: BSD-3-Clause

module Test.VCard.Types.Param (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.VCard.Types.Param.Generic qualified

tests :: TestTree
tests =
  testGroup
    "Param"
    [ Test.VCard.Types.Param.Generic.tests
    ]
