-- SPDX-FileCopyrightText: Copyright Preetham Gujjula
-- SPDX-License-Identifier: BSD-3-Clause

module Test.VCard.Types.Param (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.VCard.Types.Param.Generic qualified
import Test.VCard.Types.Param.Language qualified
import Test.VCard.Types.Param.Pref qualified
import Test.VCard.Types.Param.Value qualified

tests :: TestTree
tests =
  testGroup
    "Param"
    [ Test.VCard.Types.Param.Generic.tests,
      Test.VCard.Types.Param.Language.tests,
      Test.VCard.Types.Param.Pref.tests,
      Test.VCard.Types.Param.Value.tests
    ]
