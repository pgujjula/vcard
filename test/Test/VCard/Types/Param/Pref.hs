-- SPDX-FileCopyrightText: Copyright Preetham Gujjula
-- SPDX-License-Identifier: BSD-3-Clause
{-# LANGUAGE OverloadedStrings #-}

module Test.VCard.Types.Param.Pref (tests) where

import Data.Finite (finite)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))
import VCard.Parse (parse)
import VCard.Serialize (serialize)
import VCard.Types.Param.Generic (Param (..))
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
        Param
          { paramName = CaseInsensitiveUpper (symbolSing @"PREF"),
            paramValue = PrefValue (finite 0)
          }
    (parse "pref=2" @?=) $
      Just $
        Param
          { paramName = CaseInsensitiveUpper (symbolSing @"pref"),
            paramValue = PrefValue (finite 1)
          }
    (parse "Pref=99" @?=) $
      Just $
        Param
          { paramName = CaseInsensitiveUpper (symbolSing @"Pref"),
            paramValue = PrefValue (finite 98)
          }
    (parse "prEf=100" @?=) $
      Just $
        Param
          { paramName = CaseInsensitiveUpper (symbolSing @"prEf"),
            paramValue = PrefValue (finite 99)
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
      ( Param
          { paramName = CaseInsensitiveUpper (symbolSing @"PREF"),
            paramValue = PrefValue (finite 0)
          }
      )
      @?= "PREF=1"
    serialize
      ( Param
          { paramName = CaseInsensitiveUpper (symbolSing @"pref"),
            paramValue = PrefValue (finite 1)
          }
      )
      @?= "pref=2"
    serialize
      ( Param
          { paramName = CaseInsensitiveUpper (symbolSing @"Pref"),
            paramValue = PrefValue (finite 98)
          }
      )
      @?= "Pref=99"
    serialize
      ( Param
          { paramName = CaseInsensitiveUpper (symbolSing @"prEf"),
            paramValue = PrefValue (finite 99)
          }
      )
      @?= "prEf=100"
