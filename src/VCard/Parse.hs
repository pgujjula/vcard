-- SPDX-FileCopyrightText: Copyright Preetham Gujjula
-- SPDX-License-Identifier: BSD-3-Clause

-- | Support for parsing VCards
module VCard.Parse (Parser, HasParser (..)) where

import Data.Text (Text)
import Data.Void (Void)
import Text.Megaparsec (Parsec)

type Parser = Parsec Void Text

class HasParser a where
  parser :: Parser a
