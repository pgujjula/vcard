-- SPDX-FileCopyrightText: Copyright Preetham Gujjula
-- SPDX-License-Identifier: BSD-3-Clause
{-# LANGUAGE OverloadedStrings #-}

module Test.VCard.Types.Param.Calscale (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))
import VCard.Parse (parse)
import VCard.Serialize (serialize)
import VCard.Types.Param.Calscale (Calscale, CalscaleValue (..))
import VCard.Types.Param.Generic (GenericParam (..))
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
        GenericParam
          { genericParamName = CaseInsensitiveUpper (symbolSing @"CALSCALE"),
            genericParamValue =
              CalscaleValue (CaseInsensitiveLower (symbolSing @"gregorian"))
          }
    (parse "calscale=GREGORIAN" @?=) $
      Just $
        GenericParam
          { genericParamName = CaseInsensitiveUpper (symbolSing @"calscale"),
            genericParamValue =
              CalscaleValue (CaseInsensitiveLower (symbolSing @"GREGORIAN"))
          }
    (parse "CalScale=gregOrian" @?=) $
      Just $
        GenericParam
          { genericParamName = CaseInsensitiveUpper (symbolSing @"CalScale"),
            genericParamValue =
              CalscaleValue (CaseInsensitiveLower (symbolSing @"gregOrian"))
          }
    --
    (parse "CALSCALE=x-abc" @?=) $
      Just $
        GenericParam
          { genericParamName = CaseInsensitiveUpper (symbolSing @"CALSCALE"),
            genericParamValue =
              CalscaleValue (CaseInsensitiveLower (symbolSing @"x-abc"))
          }
    (parse "CalScale=X-ABC" @?=) $
      Just $
        GenericParam
          { genericParamName = CaseInsensitiveUpper (symbolSing @"CalScale"),
            genericParamValue =
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
      ( GenericParam
          { genericParamName = CaseInsensitiveUpper (symbolSing @"CALSCALE"),
            genericParamValue =
              CalscaleValue (CaseInsensitiveLower (symbolSing @"gregorian"))
          }
      )
      @?= "CALSCALE=gregorian"
    serialize
      ( GenericParam
          { genericParamName = CaseInsensitiveUpper (symbolSing @"calscale"),
            genericParamValue =
              CalscaleValue (CaseInsensitiveLower (symbolSing @"GREGORIAN"))
          }
      )
      @?= "calscale=GREGORIAN"
    serialize
      ( GenericParam
          { genericParamName = CaseInsensitiveUpper (symbolSing @"CalScale"),
            genericParamValue =
              CalscaleValue (CaseInsensitiveLower (symbolSing @"gregOrian"))
          }
      )
      @?= "CalScale=gregOrian"
    --
    serialize
      ( GenericParam
          { genericParamName = CaseInsensitiveUpper (symbolSing @"CALSCALE"),
            genericParamValue =
              CalscaleValue (CaseInsensitiveLower (symbolSing @"x-abc"))
          }
      )
      @?= "CALSCALE=x-abc"
    serialize
      ( GenericParam
          { genericParamName = CaseInsensitiveUpper (symbolSing @"CalScale"),
            genericParamValue =
              CalscaleValue (CaseInsensitiveLower (symbolSing @"X-ABC"))
          }
      )
      @?= "CalScale=X-ABC"
