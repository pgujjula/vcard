-- SPDX-FileCopyrightText: Copyright Preetham Gujjula
-- SPDX-License-Identifier: BSD-3-Clause
{-# LANGUAGE OverloadedStrings #-}

module Test.VCard.Types.VCard (tests) where

import Data.Text (Text)
import Data.Text qualified as Text
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))
import VCard.Parse (parse)
import VCard.Serialize (serialize)
import VCard.Types.Property.Version (Version (..))
import VCard.Types.VCard.Internal (VCard (..), VCardEntity (..))

tests :: TestTree
tests =
  testGroup
    "VCard"
    [ vCardTests,
      vCardEntityTests
    ]

vCardTests :: TestTree
vCardTests = testCase "VCard" $ do
  parse testVCardText @?= Just testVCard
  serialize testVCard @?= testVCardText

vCardEntityTests :: TestTree
vCardEntityTests = testCase "VCardEntity" $ do
  parse testVCardEntityText @?= Just testVCardEntity
  serialize testVCardEntity @?= testVCardEntityText

testVCard :: VCard
testVCard =
  VCard
    { vCardVersion = Version_4_0
    }

testVCardText :: Text
testVCardText =
  Text.concat
    [ "BEGIN:VCARD\r\n",
      "VERSION:4.0\r\n",
      "END:VCARD\r\n"
    ]

testVCardEntity :: VCardEntity
testVCardEntity = VCardEntity [testVCard]

testVCardEntityText :: Text
testVCardEntityText = testVCardText
