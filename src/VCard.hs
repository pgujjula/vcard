-- SPDX-FileCopyrightText: Copyright Preetham Gujjula
-- SPDX-License-Identifier: BSD-3-Clause
{-# LANGUAGE OverloadedStrings #-}

module VCard (parse, serialize) where

import Data.Function ((&))
import Data.List.NonEmpty qualified as NonEmpty
import Data.Text (Text, pack)
import Data.Text qualified as Text
import Text.Megaparsec (parseMaybe)
import VCard.Types (FN (..), VCard (..), VCardEntity (..), vCardEntityParser)
import VCard.Util (crlf)

--
-- Parsing
--
parse :: Text -> Maybe VCardEntity
parse = parseMaybe vCardEntityParser

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
