-- SPDX-FileCopyrightText: Copyright Preetham Gujjula
-- SPDX-License-Identifier: BSD-3-Clause
{-# LANGUAGE OverloadedStrings #-}

module Test.VCard (tests) where

import Data.List.NonEmpty (NonEmpty ((:|)))
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))
import VCard
import VCard.Types

tests :: TestTree
tests =
  testGroup
    "VCard"
    [ serializeTests
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
