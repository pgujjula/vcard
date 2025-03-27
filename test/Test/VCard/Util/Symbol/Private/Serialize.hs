-- SPDX-FileCopyrightText: Copyright Preetham Gujjula
-- SPDX-License-Identifier: BSD-3-Clause
{-# LANGUAGE OverloadedStrings #-}

module Test.VCard.Util.Symbol.Private.Serialize (tests) where

import Test.Tasty (TestTree)
import Test.Tasty.HUnit (testCase, (@?=))
import VCard.Serialize (serialize)
import VCard.Util.Symbol.Private.Compat (symbolSing)
import VCard.Util.Symbol.Private.Serialize ()

tests :: TestTree
tests = testCase "Serialize" $ do
  serialize (symbolSing @"") @?= ""
  serialize (symbolSing @"a") @?= "a"
  serialize (symbolSing @"foo bar") @?= "foo bar"
