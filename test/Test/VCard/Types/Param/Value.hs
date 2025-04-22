-- SPDX-FileCopyrightText: Copyright Preetham Gujjula
-- SPDX-License-Identifier: BSD-3-Clause
{-# LANGUAGE OverloadedStrings #-}

module Test.VCard.Types.Param.Value (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))
import VCard.Parse (parse)
import VCard.Serialize (serialize)
import VCard.Types.Param.Generic (GenericParam (..))
import VCard.Types.Param.Value (ValueParam, ValueValue (..))
import VCard.Types.Textual
  ( CaseInsensitiveLower (..),
    CaseInsensitiveUpper (..),
  )
import VCard.Util.Symbol (symbolSing)

tests :: TestTree
tests =
  testGroup
    "Value"
    [ test_parse,
      test_serialize
    ]

test_parse :: TestTree
test_parse =
  testCase "parse" $ do
    (parse "VALUE=text" @?=) $
      Just $
        GenericParam
          { genericParamName = CaseInsensitiveUpper (symbolSing @"VALUE"),
            genericParamValue = ValueValue (CaseInsensitiveLower (symbolSing @"text"))
          }
    (parse "value=uri" @?=) $
      Just $
        GenericParam
          { genericParamName = CaseInsensitiveUpper (symbolSing @"value"),
            genericParamValue = ValueValue (CaseInsensitiveLower (symbolSing @"uri"))
          }
    (parse "VALUE=DATE" @?=) $
      Just $
        GenericParam
          { genericParamName = CaseInsensitiveUpper (symbolSing @"VALUE"),
            genericParamValue = ValueValue (CaseInsensitiveLower (symbolSing @"DATE"))
          }
    (parse "value=TIME" @?=) $
      Just $
        GenericParam
          { genericParamName = CaseInsensitiveUpper (symbolSing @"value"),
            genericParamValue = ValueValue (CaseInsensitiveLower (symbolSing @"TIME"))
          }
    (parse "VaLuE=date-time" @?=) $
      Just $
        GenericParam
          { genericParamName = CaseInsensitiveUpper (symbolSing @"VaLuE"),
            genericParamValue =
              ValueValue (CaseInsensitiveLower (symbolSing @"date-time"))
          }
    (parse "VALUE=dAtE-AnD-oR-tImE" @?=) $
      Just $
        GenericParam
          { genericParamName = CaseInsensitiveUpper (symbolSing @"VALUE"),
            genericParamValue =
              ValueValue (CaseInsensitiveLower (symbolSing @"dAtE-AnD-oR-tImE"))
          }
    (parse "VALUE=timestamp" @?=) $
      Just $
        GenericParam
          { genericParamName = CaseInsensitiveUpper (symbolSing @"VALUE"),
            genericParamValue =
              ValueValue (CaseInsensitiveLower (symbolSing @"timestamp"))
          }
    (parse "VALUE=boolean" @?=) $
      Just $
        GenericParam
          { genericParamName = CaseInsensitiveUpper (symbolSing @"VALUE"),
            genericParamValue =
              ValueValue (CaseInsensitiveLower (symbolSing @"boolean"))
          }
    (parse "VALUE=integer" @?=) $
      Just $
        GenericParam
          { genericParamName = CaseInsensitiveUpper (symbolSing @"VALUE"),
            genericParamValue =
              ValueValue (CaseInsensitiveLower (symbolSing @"integer"))
          }
    (parse "VALUE=float" @?=) $
      Just $
        GenericParam
          { genericParamName = CaseInsensitiveUpper (symbolSing @"VALUE"),
            genericParamValue = ValueValue (CaseInsensitiveLower (symbolSing @"float"))
          }
    (parse "VALUE=utc-offset" @?=) $
      Just $
        GenericParam
          { genericParamName = CaseInsensitiveUpper (symbolSing @"VALUE"),
            genericParamValue =
              ValueValue (CaseInsensitiveLower (symbolSing @"utc-offset"))
          }
    (parse "VALUE=language-tag" @?=) $
      Just $
        GenericParam
          { genericParamName = CaseInsensitiveUpper (symbolSing @"VALUE"),
            genericParamValue =
              ValueValue (CaseInsensitiveLower (symbolSing @"language-tag"))
          }
    parse "VALUE =text" @?= (Nothing :: Maybe (ValueParam "text"))
    parse "VALUE= text" @?= (Nothing :: Maybe (ValueParam "text"))
    parse " VALUE=text" @?= (Nothing :: Maybe (ValueParam "text"))
    parse "VALUE=text " @?= (Nothing :: Maybe (ValueParam "text"))
    parse "VALUE=text;" @?= (Nothing :: Maybe (ValueParam "text"))
    parse "VALUE=text:" @?= (Nothing :: Maybe (ValueParam "text"))
    parse "VALUE=text" @?= (Nothing :: Maybe (ValueParam "uri"))

test_serialize :: TestTree
test_serialize =
  testCase "serialize" $ do
    serialize
      ( GenericParam
          { genericParamName = CaseInsensitiveUpper (symbolSing @"VALUE"),
            genericParamValue = ValueValue (CaseInsensitiveLower (symbolSing @"text"))
          }
      )
      @?= "VALUE=text"

    serialize
      ( GenericParam
          { genericParamName = CaseInsensitiveUpper (symbolSing @"value"),
            genericParamValue = ValueValue (CaseInsensitiveLower (symbolSing @"uri"))
          }
      )
      @?= "value=uri"

    serialize
      ( GenericParam
          { genericParamName = CaseInsensitiveUpper (symbolSing @"VALUE"),
            genericParamValue = ValueValue (CaseInsensitiveLower (symbolSing @"DATE"))
          }
      )
      @?= "VALUE=DATE"

    serialize
      ( GenericParam
          { genericParamName = CaseInsensitiveUpper (symbolSing @"value"),
            genericParamValue = ValueValue (CaseInsensitiveLower (symbolSing @"TIME"))
          }
      )
      @?= "value=TIME"

    serialize
      ( GenericParam
          { genericParamName = CaseInsensitiveUpper (symbolSing @"VaLuE"),
            genericParamValue =
              ValueValue (CaseInsensitiveLower (symbolSing @"date-time"))
          }
      )
      @?= "VaLuE=date-time"

    serialize
      ( GenericParam
          { genericParamName = CaseInsensitiveUpper (symbolSing @"VALUE"),
            genericParamValue =
              ValueValue (CaseInsensitiveLower (symbolSing @"dAtE-AnD-oR-tImE"))
          }
      )
      @?= "VALUE=dAtE-AnD-oR-tImE"

    serialize
      ( GenericParam
          { genericParamName = CaseInsensitiveUpper (symbolSing @"VALUE"),
            genericParamValue =
              ValueValue (CaseInsensitiveLower (symbolSing @"timestamp"))
          }
      )
      @?= "VALUE=timestamp"

    serialize
      ( GenericParam
          { genericParamName = CaseInsensitiveUpper (symbolSing @"VALUE"),
            genericParamValue =
              ValueValue (CaseInsensitiveLower (symbolSing @"boolean"))
          }
      )
      @?= "VALUE=boolean"

    serialize
      ( GenericParam
          { genericParamName = CaseInsensitiveUpper (symbolSing @"VALUE"),
            genericParamValue =
              ValueValue (CaseInsensitiveLower (symbolSing @"integer"))
          }
      )
      @?= "VALUE=integer"

    serialize
      ( GenericParam
          { genericParamName = CaseInsensitiveUpper (symbolSing @"VALUE"),
            genericParamValue = ValueValue (CaseInsensitiveLower (symbolSing @"float"))
          }
      )
      @?= "VALUE=float"

    serialize
      ( GenericParam
          { genericParamName = CaseInsensitiveUpper (symbolSing @"VALUE"),
            genericParamValue =
              ValueValue (CaseInsensitiveLower (symbolSing @"utc-offset"))
          }
      )
      @?= "VALUE=utc-offset"

    serialize
      ( GenericParam
          { genericParamName = CaseInsensitiveUpper (symbolSing @"VALUE"),
            genericParamValue =
              ValueValue (CaseInsensitiveLower (symbolSing @"language-tag"))
          }
      )
      @?= "VALUE=language-tag"
