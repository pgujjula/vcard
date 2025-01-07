-- SPDX-FileCopyrightText: Copyright Preetham Gujjula
-- SPDX-License-Identifier: BSD-3-Clause

-- |
-- Module     : VCard
-- Copyright  : Copyright Preetham Gujjula
-- License    : BSD-3-Clause
-- Maintainer : Preetham Gujjula <libraries@mail.preetham.io>
-- Stability  : experimental
--
-- Entry point for the vcard library.
module VCard
  ( -- * VCard
    VCard,
    parseVCard,
    serializeVCard,
    version,

    -- * VCardEntity
    VCardEntity (..),
    parseVCardEntity,
    serializeVCardEntity,
  )
where

import Data.Text (Text)
import Text.Megaparsec (parseMaybe)
import VCard.Parse (parser)
import VCard.Serialize (serialize)
import VCard.Types.VCard
  ( VCard,
    VCardEntity (..),
    version,
  )

-- | Parse a single contact from a strict 'Data.Text.Text'.
parseVCard :: Text -> Maybe VCard
parseVCard = parseMaybe parser

-- | Serialize a single contact to a strict 'Data.Text.Text'.
serializeVCard :: VCard -> Text
serializeVCard = serialize

-- | Parse a collection of contacts from a strict 'Data.Text.Text'.
parseVCardEntity :: Text -> Maybe VCardEntity
parseVCardEntity = parseMaybe parser

-- | Serialize a collection of contacts to a strict 'Data.Text.Text'.
serializeVCardEntity :: VCardEntity -> Text
serializeVCardEntity = serialize
