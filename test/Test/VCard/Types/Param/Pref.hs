-- SPDX-FileCopyrightText: Copyright Preetham Gujjula
-- SPDX-License-Identifier: BSD-3-Clause
{-# LANGUAGE OverloadedStrings #-}

module Test.VCard.Types.Param.Pref (tests) where

import Data.Finite (finite)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))
import VCard.Parse (parse)
import VCard.Serialize (serialize)
import VCard.Types.Param.Generic (GenericParam (..))
import VCard.Types.Param.Pref (Pref, PrefValue (..))
import VCard.Types.Textual (CaseInsensitiveUpper (..))
import VCard.Util.Symbol (symbolSing)

tests :: TestTree
tests =
  testGroup
    "Pref"
    [ test_parse,
      test_serialize
    ]

test_parse :: TestTree
test_parse =
  testCase "parse" $ do
    (parse "PREF=1" @?=) $
      Just $
        GenericParam
          { genericParamName = CaseInsensitiveUpper (symbolSing @"PREF"),
            genericParamValue = PrefValue (finite 0)
          }
    (parse "pref=2" @?=) $
      Just $
        GenericParam
          { genericParamName = CaseInsensitiveUpper (symbolSing @"pref"),
            genericParamValue = PrefValue (finite 1)
          }
    (parse "Pref=99" @?=) $
      Just $
        GenericParam
          { genericParamName = CaseInsensitiveUpper (symbolSing @"Pref"),
            genericParamValue = PrefValue (finite 98)
          }
    (parse "prEf=100" @?=) $
      Just $
        GenericParam
          { genericParamName = CaseInsensitiveUpper (symbolSing @"prEf"),
            genericParamValue = PrefValue (finite 99)
          }
    --
    parse "PREF=0" @?= (Nothing :: Maybe Pref)
    parse "PREF=01" @?= (Nothing :: Maybe Pref)
    parse "PREF=002" @?= (Nothing :: Maybe Pref)
    parse "PREF=0099" @?= (Nothing :: Maybe Pref)
    parse "PREF=0100" @?= (Nothing :: Maybe Pref)
    --
    parse "PREF =5" @?= (Nothing :: Maybe Pref)
    parse "PREF= 5" @?= (Nothing :: Maybe Pref)
    parse " PREF=5" @?= (Nothing :: Maybe Pref)
    parse "PREF=5 " @?= (Nothing :: Maybe Pref)
    parse "PREF=5;" @?= (Nothing :: Maybe Pref)
    parse "PREF=5:" @?= (Nothing :: Maybe Pref)

test_serialize :: TestTree
test_serialize =
  testCase "serialize" $ do
    serialize
      ( GenericParam
          { genericParamName = CaseInsensitiveUpper (symbolSing @"PREF"),
            genericParamValue = PrefValue (finite 0)
          }
      )
      @?= "PREF=1"
    serialize
      ( GenericParam
          { genericParamName = CaseInsensitiveUpper (symbolSing @"pref"),
            genericParamValue = PrefValue (finite 1)
          }
      )
      @?= "pref=2"
    serialize
      ( GenericParam
          { genericParamName = CaseInsensitiveUpper (symbolSing @"Pref"),
            genericParamValue = PrefValue (finite 98)
          }
      )
      @?= "Pref=99"
    serialize
      ( GenericParam
          { genericParamName = CaseInsensitiveUpper (symbolSing @"prEf"),
            genericParamValue = PrefValue (finite 99)
          }
      )
      @?= "prEf=100"
