-- SPDX-FileCopyrightText: Copyright Preetham Gujjula
-- SPDX-License-Identifier: BSD-3-Clause

module Test.VCard.Types.Param (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.VCard.Types.Param.AltID qualified
import Test.VCard.Types.Param.Any qualified
import Test.VCard.Types.Param.Calscale qualified
import Test.VCard.Types.Param.Generic qualified
import Test.VCard.Types.Param.Geo qualified
import Test.VCard.Types.Param.Language qualified
import Test.VCard.Types.Param.PID qualified
import Test.VCard.Types.Param.ParamValue qualified
import Test.VCard.Types.Param.Pref qualified
import Test.VCard.Types.Param.SortAs qualified
import Test.VCard.Types.Param.Type qualified
import Test.VCard.Types.Param.Tz qualified
import Test.VCard.Types.Param.Value qualified

tests :: TestTree
tests =
  testGroup
    "Param"
    [ Test.VCard.Types.Param.AltID.tests,
      Test.VCard.Types.Param.Any.tests,
      Test.VCard.Types.Param.Calscale.tests,
      Test.VCard.Types.Param.Generic.tests,
      Test.VCard.Types.Param.Geo.tests,
      Test.VCard.Types.Param.Language.tests,
      Test.VCard.Types.Param.ParamValue.tests,
      Test.VCard.Types.Param.PID.tests,
      Test.VCard.Types.Param.Pref.tests,
      Test.VCard.Types.Param.SortAs.tests,
      Test.VCard.Types.Param.Type.tests,
      Test.VCard.Types.Param.Tz.tests,
      Test.VCard.Types.Param.Value.tests
    ]
