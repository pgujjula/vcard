-- SPDX-FileCopyrightText: Copyright Preetham Gujjula
-- SPDX-License-Identifier: BSD-3-Clause

module Test.VCard.Types.Textual (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.VCard.Types.Textual.Private.AlphaNumDash qualified
import Test.VCard.Types.Textual.Private.CaseInsensitive qualified
import Test.VCard.Types.Textual.Private.XName qualified

tests :: TestTree
tests =
  testGroup
    "Textual"
    [ Test.VCard.Types.Textual.Private.AlphaNumDash.tests,
      Test.VCard.Types.Textual.Private.CaseInsensitive.tests,
      Test.VCard.Types.Textual.Private.XName.tests
    ]
