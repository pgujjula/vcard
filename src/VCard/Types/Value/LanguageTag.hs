-- SPDX-FileCopyrightText: Copyright Preetham Gujjula
-- SPDX-License-Identifier: BSD-3-Clause
{-# LANGUAGE InstanceSigs #-}

module VCard.Types.Value.LanguageTag
  ( LanguageTag (..),
  )
where

import Data.BCP47 (BCP47)
import Data.BCP47 qualified as BCP47
import Data.Text (Text)
import Text.Megaparsec (match)
import VCard.Parse (HasParser, Parser, parser)
import VCard.Serialize (HasSerializer, Serializer, serializer)
import Prelude hiding (Integer)

data LanguageTag = LanguageTag
  { languageTagText :: Text,
    languageTagBCP47 :: BCP47
  }
  deriving (Eq, Ord, Show)

instance HasParser LanguageTag where
  parser :: Parser LanguageTag
  parser = uncurry LanguageTag <$> match BCP47.parser

instance HasSerializer LanguageTag where
  serializer :: Serializer LanguageTag
  serializer = languageTagText
