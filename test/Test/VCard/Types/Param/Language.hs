-- SPDX-FileCopyrightText: Copyright Preetham Gujjula
-- SPDX-License-Identifier: BSD-3-Clause
{-# LANGUAGE OverloadedStrings #-}

module Test.VCard.Types.Param.Language (tests) where

import Control.Monad (forM_)
import Data.Text (Text)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))
import VCard.Parse (parse)
import VCard.Serialize (serialize)
import VCard.Types.Param.Generic (GenericParam (..))
import VCard.Types.Param.Language (Language)
import VCard.Types.Textual (CaseInsensitiveUpper (..))
import VCard.Types.Value.LanguageTag (LanguageTag (..))
import VCard.Util.Symbol (symbolSing)

tests :: TestTree
tests =
  testGroup
    "Language"
    [ test_parse,
      test_serialize
    ]

test_parse :: TestTree
test_parse =
  testCase "parse" $ do
    forM_ units_valid $ \(text, value) -> parse text @?= Just value
    forM_ units_invalid $ \text -> parse @Language text @?= Nothing

test_serialize :: TestTree
test_serialize = testCase "serialize" $
  forM_ units_valid $
    \(text, value) -> serialize value @?= text

units_valid :: [(Text, Language)]
units_valid =
  [ ( "LANGUAGE=en",
      GenericParam
        (CaseInsensitiveUpper (symbolSing @"LANGUAGE"))
        (LanguageTag "en")
    ),
    ( "language=EN-gb",
      GenericParam
        (CaseInsensitiveUpper (symbolSing @"language"))
        (LanguageTag "EN-gb")
    )
  ]

units_invalid :: [Text]
units_invalid = ["LANGUAGE="]
