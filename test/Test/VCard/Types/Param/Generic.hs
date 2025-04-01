-- SPDX-FileCopyrightText: Copyright Preetham Gujjula
-- SPDX-License-Identifier: BSD-3-Clause
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Test.VCard.Types.Param.Generic (tests) where

import Data.Finite (finite)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))
import VCard.Parse (HasParser, Parser, parse, parser)
import VCard.Serialize (HasSerializer, Serializer, serialize, serializer)
import VCard.Types.Param.Generic (GenericParam (..), mkParamParser, mkParamSerializer)
import VCard.Types.Textual (CaseInsensitiveUpper (..))
import VCard.Types.Value.Integer (Integer (..), IntegerValue (..))
import VCard.Util.Symbol (symbolSing)
import Prelude hiding (Integer)

tests :: TestTree
tests =
  testGroup
    "Generic"
    [ test_parse,
      test_serialize
    ]

type TestParam = GenericParam "TEST" Integer

instance HasParser (GenericParam "TEST" Integer) where
  parser :: Parser TestParam
  parser = mkParamParser (parser @Integer)

instance HasSerializer (GenericParam "TEST" Integer) where
  serializer :: Serializer TestParam
  serializer = mkParamSerializer (serializer @Integer)

test_parse :: TestTree
test_parse =
  testCase "parse" $ do
    parse @(GenericParam "TEST" Integer) "TEST=0123"
      @?= Just
        ( GenericParam
            (CaseInsensitiveUpper (symbolSing @"TEST"))
            (Integer 1 (Unsigned (finite 123)))
        )
    parse @(GenericParam "TEST" Integer) "Test=0123"
      @?= Just
        ( GenericParam
            (CaseInsensitiveUpper (symbolSing @"Test"))
            (Integer 1 (Unsigned (finite 123)))
        )
    parse @(GenericParam "TEST" Integer) "test=0123"
      @?= Just
        ( GenericParam
            (CaseInsensitiveUpper (symbolSing @"test"))
            (Integer 1 (Unsigned (finite 123)))
        )
    --
    parse @(GenericParam "TEST" Integer) "TEST =0123" @?= Nothing
    parse @(GenericParam "TEST" Integer) "TEST= 0123" @?= Nothing

test_serialize :: TestTree
test_serialize =
  testCase "serialize" $ do
    serialize
      ( GenericParam
          (CaseInsensitiveUpper (symbolSing @"TEST"))
          (Integer 1 (Unsigned (finite 123)))
      )
      @?= "TEST=0123"
    serialize
      ( GenericParam
          (CaseInsensitiveUpper (symbolSing @"Test"))
          (Integer 1 (Unsigned (finite 123)))
      )
      @?= "Test=0123"
    serialize
      ( GenericParam
          (CaseInsensitiveUpper (symbolSing @"test"))
          (Integer 1 (Unsigned (finite 123)))
      )
      @?= "test=0123"
