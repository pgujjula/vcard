-- SPDX-FileCopyrightText: Copyright Preetham Gujjula
-- SPDX-License-Identifier: BSD-3-Clause

module Test.VCard.Util.Symbol (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.VCard.Util.Symbol.Private.Append qualified
import Test.VCard.Util.Symbol.Private.Case qualified
import Test.VCard.Util.Symbol.Private.Compat qualified
import Test.VCard.Util.Symbol.Private.Length qualified
import Test.VCard.Util.Symbol.Private.List qualified
import Test.VCard.Util.Symbol.Private.Parse qualified
import Test.VCard.Util.Symbol.Private.Prefix qualified
import Test.VCard.Util.Symbol.Private.Serialize qualified
import Test.VCard.Util.Symbol.Private.Singleton qualified
import Test.VCard.Util.Symbol.Private.Slice qualified
import Test.VCard.Util.Symbol.Private.Snoc qualified

tests :: TestTree
tests =
  testGroup
    "Symbol"
    [ Test.VCard.Util.Symbol.Private.Append.tests,
      Test.VCard.Util.Symbol.Private.Case.tests,
      Test.VCard.Util.Symbol.Private.Compat.tests,
      Test.VCard.Util.Symbol.Private.Length.tests,
      Test.VCard.Util.Symbol.Private.List.tests,
      Test.VCard.Util.Symbol.Private.Parse.tests,
      Test.VCard.Util.Symbol.Private.Prefix.tests,
      Test.VCard.Util.Symbol.Private.Serialize.tests,
      Test.VCard.Util.Symbol.Private.Singleton.tests,
      Test.VCard.Util.Symbol.Private.Slice.tests,
      Test.VCard.Util.Symbol.Private.Snoc.tests
    ]
