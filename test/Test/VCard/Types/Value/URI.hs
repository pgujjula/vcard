-- SPDX-FileCopyrightText: Copyright Preetham Gujjula
-- SPDX-License-Identifier: BSD-3-Clause
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Test.VCard.Types.Value.URI (tests) where

import Data.GenValidity.URI (genURI)
import Data.Text (Text)
import Data.Text qualified as Text
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck (choose, discard, elements, testProperty, (===))
import Text.Megaparsec (parseMaybe)
import Text.Megaparsec.Char (string)
import VCard.Parse (parse, parser)
import VCard.Serialize (serialize)
import VCard.Types.Value.URI (URI)

tests :: TestTree
tests =
  testGroup
    "URI"
    [ test_URI_valid,
      test_URI_invalid
    ]

test_URI_valid :: TestTree
test_URI_valid =
  testGroup
    "valid"
    [ test_URI_valid_complete,
      test_URI_valid_prefix
    ]

test_URI_valid_complete :: TestTree
test_URI_valid_complete =
  testProperty "parse_entire_string" $ do
    uriText <- Text.pack . show <$> genURI
    pure $ fmap serialize (parse @URI uriText) === Just uriText

test_URI_valid_prefix :: TestTree
test_URI_valid_prefix =
  testProperty "parse_prefix" $ do
    uriText <- Text.pack . show <$> genURI
    suffix <- elements nonURITexts
    let fullText = uriText <> suffix
    pure $
      fmap serialize (parseMaybe (parser @URI <* string suffix) fullText)
        === Just uriText

test_URI_invalid :: TestTree
test_URI_invalid =
  testGroup
    "invalid"
    [ test_URI_invalid_strayPrefix,
      test_URI_invalid_strayInfix,
      test_URI_invalid_straySuffix
    ]

test_URI_invalid_strayPrefix :: TestTree
test_URI_invalid_strayPrefix =
  testProperty "stray_prefix" $ do
    uriText <- Text.pack . show <$> genURI
    prefix <- elements nonURITexts
    let invalidURI = prefix <> uriText
    pure $ parseMaybe (parser @URI) invalidURI === Nothing

test_URI_invalid_strayInfix :: TestTree
test_URI_invalid_strayInfix =
  testProperty "stray_infix" $ do
    uriText <- Text.pack . show <$> genURI
    infix' <- elements nonURITexts
    if Text.length uriText < 2
      then pure discard
      else do
        middleIndex <- choose (1, Text.length uriText - 1)
        let invalidURI =
              Text.take middleIndex uriText
                <> infix'
                <> Text.drop middleIndex uriText
        pure $ parseMaybe (parser @URI) invalidURI === Nothing

test_URI_invalid_straySuffix :: TestTree
test_URI_invalid_straySuffix =
  testProperty "stray_suffix" $ do
    uriText <- Text.pack . show <$> genURI
    suffix <- elements nonURITexts
    let invalidURI = uriText <> suffix
    pure $ parseMaybe (parser @URI) invalidURI === Nothing

-- Characters/character sequences that cannot appear in URIs
nonURITexts :: [Text]
nonURITexts = ["\n", " ", "\r\n", "\""]
