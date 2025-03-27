-- SPDX-FileCopyrightText: Copyright Preetham Gujjula
-- SPDX-License-Identifier: BSD-3-Clause
{-# LANGUAGE OverloadedStrings #-}

module Test.VCard.Types.Param.Calscale (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))
import VCard.Parse (parse)
import VCard.Serialize (serialize)
import VCard.Types.Param.Calscale (Calscale, CalscaleValue (..))
import VCard.Types.Param.Generic (Param (..))
import VCard.Types.Textual
  ( CaseInsensitiveLower (..),
    CaseInsensitiveUpper (..),
  )
import VCard.Util.Symbol (symbolSing)

tests :: TestTree
tests =
  testGroup
    "Calscale"
    [ test_parse,
      test_serialize
    ]

test_parse :: TestTree
test_parse =
  testCase "parse" $ do
    (parse "CALSCALE=gregorian" @?=) $
      Just $
        Param
          { paramName = CaseInsensitiveUpper (symbolSing @"CALSCALE"),
            paramValue =
              CalscaleValue (CaseInsensitiveLower (symbolSing @"gregorian"))
          }
    (parse "calscale=GREGORIAN" @?=) $
      Just $
        Param
          { paramName = CaseInsensitiveUpper (symbolSing @"calscale"),
            paramValue =
              CalscaleValue (CaseInsensitiveLower (symbolSing @"GREGORIAN"))
          }
    (parse "CalScale=gregOrian" @?=) $
      Just $
        Param
          { paramName = CaseInsensitiveUpper (symbolSing @"CalScale"),
            paramValue =
              CalscaleValue (CaseInsensitiveLower (symbolSing @"gregOrian"))
          }
    --
    (parse "CALSCALE=x-abc" @?=) $
      Just $
        Param
          { paramName = CaseInsensitiveUpper (symbolSing @"CALSCALE"),
            paramValue =
              CalscaleValue (CaseInsensitiveLower (symbolSing @"x-abc"))
          }
    (parse "CalScale=X-ABC" @?=) $
      Just $
        Param
          { paramName = CaseInsensitiveUpper (symbolSing @"CalScale"),
            paramValue =
              CalscaleValue (CaseInsensitiveLower (symbolSing @"X-ABC"))
          }

    parse "CALSCALE =gregorian" @?= (Nothing :: Maybe (Calscale "gregorian"))
    parse "CALSCALE= gregorian" @?= (Nothing :: Maybe (Calscale "gregorian"))
    parse " CALSCALE=gregorian" @?= (Nothing :: Maybe (Calscale "gregorian"))
    parse "CALSCALE=gregorian " @?= (Nothing :: Maybe (Calscale "gregorian"))
    parse "CALSCALE=gregorian;" @?= (Nothing :: Maybe (Calscale "gregorian"))
    parse "CALSCALE=gregorian:" @?= (Nothing :: Maybe (Calscale "gregorian"))

    parse "CALSCALE=gregorian" @?= (Nothing :: Maybe (Calscale "x-abc"))
    parse "CALSCALE=x-abc" @?= (Nothing :: Maybe (Calscale "gregorian"))

test_serialize :: TestTree
test_serialize =
  testCase "serialize" $ do
    serialize
      ( Param
          { paramName = CaseInsensitiveUpper (symbolSing @"CALSCALE"),
            paramValue =
              CalscaleValue (CaseInsensitiveLower (symbolSing @"gregorian"))
          }
      )
      @?= "CALSCALE=gregorian"
    serialize
      ( Param
          { paramName = CaseInsensitiveUpper (symbolSing @"calscale"),
            paramValue =
              CalscaleValue (CaseInsensitiveLower (symbolSing @"GREGORIAN"))
          }
      )
      @?= "calscale=GREGORIAN"
    serialize
      ( Param
          { paramName = CaseInsensitiveUpper (symbolSing @"CalScale"),
            paramValue =
              CalscaleValue (CaseInsensitiveLower (symbolSing @"gregOrian"))
          }
      )
      @?= "CalScale=gregOrian"
    --
    serialize
      ( Param
          { paramName = CaseInsensitiveUpper (symbolSing @"CALSCALE"),
            paramValue =
              CalscaleValue (CaseInsensitiveLower (symbolSing @"x-abc"))
          }
      )
      @?= "CALSCALE=x-abc"
    serialize
      ( Param
          { paramName = CaseInsensitiveUpper (symbolSing @"CalScale"),
            paramValue =
              CalscaleValue (CaseInsensitiveLower (symbolSing @"X-ABC"))
          }
      )
      @?= "CalScale=X-ABC"
