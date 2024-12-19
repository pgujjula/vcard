-- SPDX-FileCopyrightText: Copyright Preetham Gujjula
-- SPDX-License-Identifier: BSD-3-Clause
{-# LANGUAGE OverloadedStrings #-}

module Test.VCard (tests) where

import Data.List.NonEmpty (NonEmpty ((:|)))
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))
import Text.Megaparsec (parseMaybe)
import VCard
import VCard.Parse (parser)
import VCard.Serialize (serializer)
import VCard.Types

tests :: TestTree
tests =
  testGroup
    "VCard"
    [ serializeTests,
      valueParamTests
    ]

serializeTests :: TestTree
serializeTests =
  testCase
    "parse/serialize round-trip"
    $ parse (serialize testVCardEntity) @?= Just testVCardEntity

testVCardEntity :: VCardEntity
testVCardEntity =
  VCardEntity $
    vcard1 :| [vcard2]

vcard1 :: VCard
vcard1 =
  VCard
    { vCardVersion = Version_4_0,
      vCardFN = FN "Doc"
    }

vcard2 :: VCard
vcard2 =
  VCard
    { vCardVersion = Version_4_0,
      vCardFN = FN "Bigfoot"
    }

valueParamTests :: TestTree
valueParamTests =
  testCase "valueParam" $ do
    parseMaybe parser "VALUE=text" @?= Just testValueParam
    serializer testValueParam @?= "VALUE=text"

testValueParam :: ValueParam
testValueParam = ValueParam VTText
