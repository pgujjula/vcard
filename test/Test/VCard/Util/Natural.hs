-- SPDX-FileCopyrightText: Copyright Preetham Gujjula
-- SPDX-License-Identifier: BSD-3-Clause
module Test.VCard.Util.Natural (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.VCard.Util.Natural.Private.Compat qualified

tests :: TestTree
tests =
  testGroup
    "Natural"
    [ Test.VCard.Util.Natural.Private.Compat.tests
    ]
