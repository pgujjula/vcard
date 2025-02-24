-- SPDX-FileCopyrightText: Copyright Preetham Gujjula
-- SPDX-License-Identifier: BSD-3-Clause
module Test.VCard.Natural.Private (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.VCard.Natural.Private.Compat qualified

tests :: TestTree
tests =
  testGroup
    "Natural"
    [ Test.VCard.Natural.Private.Compat.tests
    ]
