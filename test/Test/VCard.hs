-- SPDX-FileCopyrightText: Copyright Preetham Gujjula
-- SPDX-License-Identifier: BSD-3-Clause
{-# LANGUAGE OverloadedStrings #-}

module Test.VCard (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.VCard.Symbol qualified
import Test.VCard.Types qualified

tests :: TestTree
tests =
  testGroup
    "VCard"
    [ Test.VCard.Symbol.tests,
      Test.VCard.Types.tests
    ]
