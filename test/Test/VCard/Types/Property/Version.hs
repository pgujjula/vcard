-- SPDX-FileCopyrightText: Copyright Preetham Gujjula
-- SPDX-License-Identifier: BSD-3-Clause
{-# LANGUAGE OverloadedStrings #-}

module Test.VCard.Types.Property.Version (tests) where

import Control.Monad (forM_)
import Data.Text (Text)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))
import VCard.Parse (parse)
import VCard.Serialize (serialize)
import VCard.Types.Property.Version (Version (..))

tests :: TestTree
tests =
  testGroup
    "Version"
    [ test_Version_valid,
      test_Version_invalid
    ]

test_Version_valid :: TestTree
test_Version_valid =
  testCase "valid" $ do
    parse testVersionText @?= Just testVersion
    serialize testVersion @?= testVersionText

test_Version_invalid :: TestTree
test_Version_invalid =
  testCase "invalid" $
    forM_ invalidVersionTexts $ \t ->
      parse @Version t @?= Nothing

testVersion :: Version
testVersion = Version_4_0

testVersionText :: Text
testVersionText = "VERSION:4.0"

invalidVersionTexts :: [Text]
invalidVersionTexts =
  [ "VERSION:4.1",
    "VERSION:3.0",
    "VERSION:2.0",
    "VERSION:1.0",
    "VERSION:4.0\r\n",
    "VERSION:4.0\n",
    " VERSION:4.0",
    "version:4.0"
  ]
