-- SPDX-FileCopyrightText: Copyright Preetham Gujjula
-- SPDX-License-Identifier: BSD-3-Clause
{-# LANGUAGE OverloadedStrings #-}

module Test.VCard.Types (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.VCard.Types.Property qualified
import Test.VCard.Types.VCard qualified

tests :: TestTree
tests =
  testGroup
    "Types"
    [ Test.VCard.Types.Property.tests,
      Test.VCard.Types.VCard.tests
    ]
