-- SPDX-FileCopyrightText: Copyright Preetham Gujjula
-- SPDX-License-Identifier: BSD-3-Clause
{-# LANGUAGE InstanceSigs #-}

module VCard.Types.Value.LanguageTag
  ( LanguageTag (..),
  )
where

import Data.Char (isAlphaNum, isAscii)
import Data.Text (Text)
import Text.Megaparsec (takeWhile1P)
import VCard.Parse (HasParser, Parser, parser)
import VCard.Serialize (HasSerializer, Serializer, serializer)
import Prelude hiding (Integer)

newtype LanguageTag = LanguageTag
  { unLanguageTag :: Text
  }
  deriving (Eq, Ord, Show)

instance HasParser LanguageTag where
  parser :: Parser LanguageTag
  parser = LanguageTag <$> takeWhile1P Nothing isAlphaNumDash

isAlphaNumDash :: Char -> Bool
isAlphaNumDash c = (isAlphaNum c || c == '-') && isAscii c

instance HasSerializer LanguageTag where
  serializer :: Serializer LanguageTag
  serializer = unLanguageTag
