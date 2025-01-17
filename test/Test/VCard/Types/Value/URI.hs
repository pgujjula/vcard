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
    [ validURITests,
      invalidURITests
    ]

validURITests :: TestTree
validURITests =
  testGroup
    "valid"
    [ completeURITests,
      prefixURITests
    ]

completeURITests :: TestTree
completeURITests =
  testProperty "parse_entire_string" $ do
    uriText <- Text.pack . show <$> genURI
    pure $ fmap serialize (parse @URI uriText) === Just uriText

prefixURITests :: TestTree
prefixURITests =
  testProperty "parse_prefix" $ do
    uriText <- Text.pack . show <$> genURI
    suffix <- elements nonURITexts
    let fullText = uriText <> suffix
    pure $
      fmap serialize (parseMaybe (parser @URI <* string suffix) fullText)
        === Just uriText

invalidURITests :: TestTree
invalidURITests =
  testGroup
    "invalid"
    [ strayPrefixTests,
      strayInfixCharTests,
      straySuffixCharTests
    ]

strayPrefixTests :: TestTree
strayPrefixTests =
  testProperty "stray_prefix" $ do
    uriText <- Text.pack . show <$> genURI
    prefix <- elements nonURITexts
    let invalidURI = prefix <> uriText
    pure $ parseMaybe (parser @URI) invalidURI === Nothing

strayInfixCharTests :: TestTree
strayInfixCharTests =
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

straySuffixCharTests :: TestTree
straySuffixCharTests =
  testProperty "stray_suffix" $ do
    uriText <- Text.pack . show <$> genURI
    suffix <- elements nonURITexts
    let invalidURI = uriText <> suffix
    pure $ parseMaybe (parser @URI) invalidURI === Nothing

-- Characters/character sequences that cannot appear in URIs
nonURITexts :: [Text]
nonURITexts = ["\n", " ", "\r\n", "\""]
