-- SPDX-FileCopyrightText: Copyright Preetham Gujjula
-- SPDX-License-Identifier: BSD-3-Clause
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE OverloadedStrings #-}

module VCard.Types.Value.Time
  ( Hour (..),
    Minute (..),
    Second (..),
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
        fail $ show hourInt <> " was out of bounds for hour (00 to 23)"

instance HasSerializer Hour where
  serializer :: Serializer Hour
  serializer = Text.justifyRight 2 '0' . Text.pack . show . getFinite . unHour

--
-- Minute
--

-- | A minute of an hour, between 00 and 59.
newtype Minute = Minute {unMinute :: Finite 60}
  deriving (Eq, Show, Ord, Bounded)

instance HasParser Minute where
  parser :: Parser Minute
  parser = do
    d1 <- toDigit <$> digitChar
    d2 <- toDigit <$> digitChar
    let minuteInt = 10 * d1 + d2
    case packFinite (toInteger minuteInt) of
      Just minuteFinite -> pure (Minute minuteFinite)
      Nothing ->
        fail $ show minuteInt <> " was out of bounds for minute (00 to 59)"

instance HasSerializer Minute where
  serializer :: Serializer Minute
  serializer = Text.justifyRight 2 '0' . Text.pack . show . getFinite . unMinute

--
-- Second
--

-- | A second of an minute, usually between 00 and 59, and 60 for leap seconds.
newtype Second = Second {unSecond :: Finite 61}
  deriving (Eq, Show, Ord, Bounded)

instance HasParser Second where
  parser :: Parser Second
  parser = do
    d1 <- toDigit <$> digitChar
    d2 <- toDigit <$> digitChar
    let secondInt = 10 * d1 + d2
    case packFinite (toInteger secondInt) of
      Just secondFinite -> pure (Second secondFinite)
      Nothing ->
        fail $ show secondInt <> " was out of bounds for second (00 to 60)"

instance HasSerializer Second where
  serializer :: Serializer Second
  serializer = Text.justifyRight 2 '0' . Text.pack . show . getFinite . unSecond

-- Utilities
toDigit :: Char -> Int
toDigit c = ord c - ord '0'
