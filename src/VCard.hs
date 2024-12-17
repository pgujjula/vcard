-- SPDX-FileCopyrightText: Copyright Preetham Gujjula
-- SPDX-License-Identifier: BSD-3-Clause
{-# LANGUAGE OverloadedStrings #-}

module VCard (parse, serialize) where

import Control.Monad (void)
import Control.Monad.Combinators.NonEmpty qualified as NonEmpty
import Data.Function ((&))
import Data.List.NonEmpty qualified as NonEmpty
import Data.Text (Text, pack)
import Data.Text qualified as Text
import Text.Megaparsec (parseMaybe, takeWhileP)
import Text.Megaparsec.Char (string)
import VCard.Parse (Parser)
import VCard.Types (FN (..), VCard (..), VCardEntity (..), Version (..))
import VCard.Util (crlf)

--
-- Parsing
--
parse :: Text -> Maybe VCardEntity
parse = parseMaybe vCardEntityParser

vCardEntityParser :: Parser VCardEntity
vCardEntityParser = VCardEntity <$> NonEmpty.some vCardParser

vCardParser :: Parser VCard
vCardParser = do
  void (string ("BEGIN:VCARD" <> crlf))
  void (string ("VERSION:4.0" <> crlf))
  fn <- fnParser
  void (string ("END:VCARD" <> crlf))
  pure $
    VCard
      { vCardVersion = Version_4_0,
        vCardFN = fn
      }

fnParser :: Parser FN
fnParser = do
  void (string "FN:")
  fnText <- takeWhileP Nothing (/= '\r')
  void (string crlf)
  pure (FN fnText)

--
-- Serialization
--
serialize :: VCardEntity -> Text
serialize = serializeVCardEntity

serializeVCardEntity :: VCardEntity -> Text
serializeVCardEntity vCardEntity =
  unVCardEntity vCardEntity
    & NonEmpty.toList
    & map serializeVCard
    & Text.concat

serializeVCard :: VCard -> Text
serializeVCard vCard =
  Text.concat $
    map
      (<> crlf)
      [ pack "BEGIN:VCARD",
        pack "VERSION:4.0",
        serializeFN (vCardFN vCard),
        pack "END:VCARD"
      ]

serializeFN :: FN -> Text
serializeFN fn = Text.pack "FN:" <> unFN fn
