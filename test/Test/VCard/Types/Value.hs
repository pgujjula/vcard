-- SPDX-FileCopyrightText: Copyright Preetham Gujjula
-- SPDX-License-Identifier: BSD-3-Clause

module Test.VCard.Types.Value (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.VCard.Types.Value.Text qualified

tests :: TestTree
tests =
  testGroup
    "Value"
    [ Test.VCard.Types.Value.Text.tests
    ]
