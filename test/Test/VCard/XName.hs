-- SPDX-FileCopyrightText: Copyright Preetham Gujjula
-- SPDX-License-Identifier: BSD-3-Clause
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}

module Test.VCard.XName (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.VCard.XName.TypeTests
  ( test_XNameLowerSymbol,
    test_XNameSymbol,
    test_XNameUpperSymbol,
  )

tests :: TestTree
tests =
  testGroup
    "XName"
    [ test_XNameSymbol,
      test_XNameLowerSymbol,
      test_XNameUpperSymbol
    ]
