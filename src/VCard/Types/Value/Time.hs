-- SPDX-FileCopyrightText: Copyright Preetham Gujjula
-- SPDX-License-Identifier: BSD-3-Clause
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE OverloadedStrings #-}

module VCard.Types.Value.Time
  ( Hour (..),
  )
where

import Data.Char (ord)
import Data.Finite (Finite, getFinite, packFinite)
import Data.Text qualified as Text
import Text.Megaparsec.Char (digitChar)
import VCard.Parse (HasParser, Parser, parser)
import VCard.Serialize (HasSerializer, Serializer, serializer)

--
-- Hour
--

-- | An hour of the day, between 0 and 23.
newtype Hour = Hour {unHour :: Finite 24}
  deriving (Eq, Ord, Show, Bounded)

instance HasParser Hour where
  parser :: Parser Hour
  parser = do
    d1 <- toDigit <$> digitChar
    d2 <- toDigit <$> digitChar
    let hourInt = toInteger (10 * d1 + d2)
    case packFinite hourInt of
      Just hourFinite -> pure (Hour hourFinite)
      Nothing ->
        fail $ show hourInt <> " was out of bounds for month (00 to 23)"

instance HasSerializer Hour where
  serializer :: Serializer Hour
  serializer = Text.justifyRight 2 '0' . Text.pack . show . getFinite . unHour

-- Utilities
toDigit :: Char -> Int
toDigit c = ord c - ord '0'
