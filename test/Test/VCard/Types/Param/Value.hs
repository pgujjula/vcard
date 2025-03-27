-- SPDX-FileCopyrightText: Copyright Preetham Gujjula
-- SPDX-License-Identifier: BSD-3-Clause
{-# LANGUAGE OverloadedStrings #-}

module Test.VCard.Types.Param.Value (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))
import VCard.Parse (parse)
import VCard.Serialize (serialize)
import VCard.Symbol.Private (symbolSing)
import VCard.Types.Param.Generic (Param (..))
import VCard.Types.Param.Value (Value, ValueValue (..))
import VCard.Types.Textual.Private.CaseInsensitive
  ( CaseInsensitiveLower (..),
    CaseInsensitiveUpper (..),
  )

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
        Param
          { paramName = CaseInsensitiveUpper (symbolSing @"VALUE"),
            paramValue = ValueValue (CaseInsensitiveLower (symbolSing @"text"))
          }
    (parse "value=uri" @?=) $
      Just $
        Param
          { paramName = CaseInsensitiveUpper (symbolSing @"value"),
            paramValue = ValueValue (CaseInsensitiveLower (symbolSing @"uri"))
          }
    (parse "VALUE=DATE" @?=) $
      Just $
        Param
          { paramName = CaseInsensitiveUpper (symbolSing @"VALUE"),
            paramValue = ValueValue (CaseInsensitiveLower (symbolSing @"DATE"))
          }
    (parse "value=TIME" @?=) $
      Just $
        Param
          { paramName = CaseInsensitiveUpper (symbolSing @"value"),
            paramValue = ValueValue (CaseInsensitiveLower (symbolSing @"TIME"))
          }
    (parse "VaLuE=date-time" @?=) $
      Just $
        Param
          { paramName = CaseInsensitiveUpper (symbolSing @"VaLuE"),
            paramValue =
              ValueValue (CaseInsensitiveLower (symbolSing @"date-time"))
          }
    (parse "VALUE=dAtE-AnD-oR-tImE" @?=) $
      Just $
        Param
          { paramName = CaseInsensitiveUpper (symbolSing @"VALUE"),
            paramValue =
              ValueValue (CaseInsensitiveLower (symbolSing @"dAtE-AnD-oR-tImE"))
          }
    (parse "VALUE=timestamp" @?=) $
      Just $
        Param
          { paramName = CaseInsensitiveUpper (symbolSing @"VALUE"),
            paramValue =
              ValueValue (CaseInsensitiveLower (symbolSing @"timestamp"))
          }
    (parse "VALUE=boolean" @?=) $
      Just $
        Param
          { paramName = CaseInsensitiveUpper (symbolSing @"VALUE"),
            paramValue =
              ValueValue (CaseInsensitiveLower (symbolSing @"boolean"))
          }
    (parse "VALUE=integer" @?=) $
      Just $
        Param
          { paramName = CaseInsensitiveUpper (symbolSing @"VALUE"),
            paramValue =
              ValueValue (CaseInsensitiveLower (symbolSing @"integer"))
          }
    (parse "VALUE=float" @?=) $
      Just $
        Param
          { paramName = CaseInsensitiveUpper (symbolSing @"VALUE"),
            paramValue = ValueValue (CaseInsensitiveLower (symbolSing @"float"))
          }
    (parse "VALUE=utc-offset" @?=) $
      Just $
        Param
          { paramName = CaseInsensitiveUpper (symbolSing @"VALUE"),
            paramValue =
              ValueValue (CaseInsensitiveLower (symbolSing @"utc-offset"))
          }
    (parse "VALUE=language-tag" @?=) $
      Just $
        Param
          { paramName = CaseInsensitiveUpper (symbolSing @"VALUE"),
            paramValue =
              ValueValue (CaseInsensitiveLower (symbolSing @"language-tag"))
          }
    parse "VALUE =text" @?= (Nothing :: Maybe (Value "text"))
    parse "VALUE= text" @?= (Nothing :: Maybe (Value "text"))
    parse " VALUE=text" @?= (Nothing :: Maybe (Value "text"))
    parse "VALUE=text " @?= (Nothing :: Maybe (Value "text"))
    parse "VALUE=text;" @?= (Nothing :: Maybe (Value "text"))
    parse "VALUE=text:" @?= (Nothing :: Maybe (Value "text"))
    parse "VALUE=text" @?= (Nothing :: Maybe (Value "uri"))

test_serialize :: TestTree
test_serialize =
  testCase "serialize" $ do
    serialize
      ( Param
          { paramName = CaseInsensitiveUpper (symbolSing @"VALUE"),
            paramValue = ValueValue (CaseInsensitiveLower (symbolSing @"text"))
          }
      )
      @?= "VALUE=text"

    serialize
      ( Param
          { paramName = CaseInsensitiveUpper (symbolSing @"value"),
            paramValue = ValueValue (CaseInsensitiveLower (symbolSing @"uri"))
          }
      )
      @?= "value=uri"

    serialize
      ( Param
          { paramName = CaseInsensitiveUpper (symbolSing @"VALUE"),
            paramValue = ValueValue (CaseInsensitiveLower (symbolSing @"DATE"))
          }
      )
      @?= "VALUE=DATE"

    serialize
      ( Param
          { paramName = CaseInsensitiveUpper (symbolSing @"value"),
            paramValue = ValueValue (CaseInsensitiveLower (symbolSing @"TIME"))
          }
      )
      @?= "value=TIME"

    serialize
      ( Param
          { paramName = CaseInsensitiveUpper (symbolSing @"VaLuE"),
            paramValue =
              ValueValue (CaseInsensitiveLower (symbolSing @"date-time"))
          }
      )
      @?= "VaLuE=date-time"

    serialize
      ( Param
          { paramName = CaseInsensitiveUpper (symbolSing @"VALUE"),
            paramValue =
              ValueValue (CaseInsensitiveLower (symbolSing @"dAtE-AnD-oR-tImE"))
          }
      )
      @?= "VALUE=dAtE-AnD-oR-tImE"

    serialize
      ( Param
          { paramName = CaseInsensitiveUpper (symbolSing @"VALUE"),
            paramValue =
              ValueValue (CaseInsensitiveLower (symbolSing @"timestamp"))
          }
      )
      @?= "VALUE=timestamp"

    serialize
      ( Param
          { paramName = CaseInsensitiveUpper (symbolSing @"VALUE"),
            paramValue =
              ValueValue (CaseInsensitiveLower (symbolSing @"boolean"))
          }
      )
      @?= "VALUE=boolean"

    serialize
      ( Param
          { paramName = CaseInsensitiveUpper (symbolSing @"VALUE"),
            paramValue =
              ValueValue (CaseInsensitiveLower (symbolSing @"integer"))
          }
      )
      @?= "VALUE=integer"

    serialize
      ( Param
          { paramName = CaseInsensitiveUpper (symbolSing @"VALUE"),
            paramValue = ValueValue (CaseInsensitiveLower (symbolSing @"float"))
          }
      )
      @?= "VALUE=float"

    serialize
      ( Param
          { paramName = CaseInsensitiveUpper (symbolSing @"VALUE"),
            paramValue =
              ValueValue (CaseInsensitiveLower (symbolSing @"utc-offset"))
          }
      )
      @?= "VALUE=utc-offset"

    serialize
      ( Param
          { paramName = CaseInsensitiveUpper (symbolSing @"VALUE"),
            paramValue =
              ValueValue (CaseInsensitiveLower (symbolSing @"language-tag"))
          }
      )
      @?= "VALUE=language-tag"
