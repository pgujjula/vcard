-- SPDX-FileCopyrightText: Copyright Preetham Gujjula
-- SPDX-License-Identifier: BSD-3-Clause
{-# LANGUAGE OverloadedStrings #-}

module Test.VCard (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.VCard.Char qualified
import Test.VCard.Natural.Private qualified
import Test.VCard.Types qualified
import Test.VCard.Util qualified

tests :: TestTree
tests =
  testGroup
    "VCard"
    [ Test.VCard.Char.tests,
      Test.VCard.Natural.Private.tests,
      Test.VCard.Types.tests,
      Test.VCard.Util.tests
    ]
