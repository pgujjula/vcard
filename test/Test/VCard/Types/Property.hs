-- SPDX-FileCopyrightText: Copyright Preetham Gujjula
-- SPDX-License-Identifier: BSD-3-Clause

module Test.VCard.Types.Property (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.VCard.Types.Property.Version qualified

tests :: TestTree
tests =
  testGroup
    "Property"
    [ Test.VCard.Types.Property.Version.tests
    ]
