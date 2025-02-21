-- SPDX-FileCopyrightText: Copyright Preetham Gujjula
-- SPDX-License-Identifier: BSD-3-Clause
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Test.VCard.Types.Param.Generic (tests) where

import Data.Finite (finite)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))
import VCard.CaseInsensitive (CaseInsensitiveUpper (..))
import VCard.Parse (parse)
import VCard.Serialize (serialize)
import VCard.Symbol.Private (symbolSing)
import VCard.Types.Param (Param (..))
import VCard.Types.Value.Integer (Integer (..), IntegerValue (..))
import Prelude hiding (Integer)

tests :: TestTree
tests =
  testGroup
    "Generic"
    [ test_parse,
      test_serialize
    ]

test_parse :: TestTree
test_parse =
  testCase "parse" $ do
    parse @(Param "TEST" Integer) "TEST=0123"
      @?= Just
        ( Param
            (CaseInsensitiveUpper (symbolSing @"TEST"))
            (Integer 1 (Unsigned (finite 123)))
        )
    parse @(Param "TEST" Integer) "Test=0123"
      @?= Just
        ( Param
            (CaseInsensitiveUpper (symbolSing @"Test"))
            (Integer 1 (Unsigned (finite 123)))
        )
    parse @(Param "TEST" Integer) "test=0123"
      @?= Just
        ( Param
            (CaseInsensitiveUpper (symbolSing @"test"))
            (Integer 1 (Unsigned (finite 123)))
        )
    --
    parse @(Param "test" Integer) "test=0123" @?= Nothing
    parse @(Param "Test" Integer) "Test=0123" @?= Nothing
    --
    parse @(Param "TEST" Integer) "TEST =0123" @?= Nothing
    parse @(Param "TEST" Integer) "TEST= 0123" @?= Nothing

test_serialize :: TestTree
test_serialize =
  testCase "serialize" $ do
    serialize
      ( Param
          (CaseInsensitiveUpper (symbolSing @"TEST"))
          (Integer 1 (Unsigned (finite 123)))
      )
      @?= "TEST=0123"
    serialize
      ( Param
          (CaseInsensitiveUpper (symbolSing @"Test"))
          (Integer 1 (Unsigned (finite 123)))
      )
      @?= "Test=0123"
    serialize
      ( Param
          (CaseInsensitiveUpper (symbolSing @"test"))
          (Integer 1 (Unsigned (finite 123)))
      )
      @?= "test=0123"
