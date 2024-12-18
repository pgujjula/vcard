-- SPDX-FileCopyrightText: Copyright Preetham Gujjula
-- SPDX-License-Identifier: BSD-3-Clause

module VCard (parse, serialize) where

import Data.Text (Text)
import Text.Megaparsec (parseMaybe)
import VCard.Parse (parser)
import VCard.Serialize (serializer)
import VCard.Types (VCardEntity (..))

--
-- Parsing
--
parse :: Text -> Maybe VCardEntity
parse = parseMaybe parser

--
-- Serialization
--
serialize :: VCardEntity -> Text
serialize = serializer
