-- SPDX-FileCopyrightText: Copyright Preetham Gujjula
-- SPDX-License-Identifier: BSD-3-Clause
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Test.VCard.Symbol.Private (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.VCard.Symbol.Private.Case qualified
import Test.VCard.Symbol.Private.Compat qualified
import Test.VCard.Symbol.Private.Prefix qualified

tests :: TestTree
tests =
  testGroup
    "Symbol"
    [ Test.VCard.Symbol.Private.Case.tests,
      Test.VCard.Symbol.Private.Compat.tests,
      Test.VCard.Symbol.Private.Prefix.tests
    ]
