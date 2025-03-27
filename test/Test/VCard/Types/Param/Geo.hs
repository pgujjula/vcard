-- SPDX-FileCopyrightText: Copyright Preetham Gujjula
-- SPDX-License-Identifier: BSD-3-Clause
{-# LANGUAGE OverloadedStrings #-}

module Test.VCard.Types.Param.Geo (tests) where

import Data.Text (Text)
import Data.Text qualified as Text
import Network.URI (parseURI)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))
import VCard.Parse (parse)
import VCard.Serialize (serialize)
import VCard.Types.Param.Generic (Param (..))
import VCard.Types.Param.Geo (Geo, GeoValue (..))
import VCard.Types.Textual (CaseInsensitiveUpper (..))
import VCard.Types.Value.URI (URI)
import VCard.Util.Symbol (symbolSing)

tests :: TestTree
tests =
  testGroup
    "Geo"
    [ test_parse,
      test_serialize
    ]

test_parse :: TestTree
test_parse =
  testCase "parse" $ do
    (parse "GEO=\"https://www.proton.me\"" @?=) $
      Just $
        Param
          { paramName = CaseInsensitiveUpper (symbolSing @"GEO"),
            paramValue =
              GeoValue (parseURIUnsafe "https://www.proton.me")
          }
    parse "GEO=https://www.proton.me" @?= (Nothing :: Maybe Geo)
    parse "GEO=\"https://www.proton.me" @?= (Nothing :: Maybe Geo)
    parse "GEO=https://www.proton.me\"" @?= (Nothing :: Maybe Geo)

test_serialize :: TestTree
test_serialize =
  testCase "serialize" $ do
    serialize
      ( Param
          { paramName = CaseInsensitiveUpper (symbolSing @"GEO"),
            paramValue =
              GeoValue (parseURIUnsafe "https://www.proton.me")
          }
      )
      @?= "GEO=\"https://www.proton.me\""

parseURIUnsafe :: Text -> URI
parseURIUnsafe text =
  case parseURI (Text.unpack text) of
    Nothing -> error "parseURIUnsafe: no parse"
    Just uri -> uri
