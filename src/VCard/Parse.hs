-- SPDX-FileCopyrightText: Copyright Preetham Gujjula
-- SPDX-License-Identifier: BSD-3-Clause

module VCard.Parse (Parser, HasParser (..), parse) where

import Data.Text (Text)
import Data.Void (Void)
import Text.Megaparsec (Parsec, parseMaybe)

-- | A megaparsec-based parser type.
type Parser a = Parsec Void Text a

-- | A class for components of a vCard that can be parsed. Most types in this
-- library implement this class.
class HasParser a where
  parser :: Parser a

parse :: (HasParser a) => Text -> Maybe a
parse = parseMaybe parser
