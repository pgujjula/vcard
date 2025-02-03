-- SPDX-FileCopyrightText: Copyright Preetham Gujjula
-- SPDX-License-Identifier: BSD-3-Clause

module Test.VCard.Types.Value (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.VCard.Types.Value.Boolean qualified
import Test.VCard.Types.Value.Date qualified
import Test.VCard.Types.Value.Integer qualified
import Test.VCard.Types.Value.Text qualified
import Test.VCard.Types.Value.Time qualified
import Test.VCard.Types.Value.URI qualified

tests :: TestTree
tests =
  testGroup
    "Value"
    [ Test.VCard.Types.Value.Text.tests,
      Test.VCard.Types.Value.URI.tests,
      Test.VCard.Types.Value.Date.tests,
      Test.VCard.Types.Value.Time.tests,
      Test.VCard.Types.Value.Boolean.tests,
      Test.VCard.Types.Value.Integer.tests
    ]
