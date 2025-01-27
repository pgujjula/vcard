-- SPDX-FileCopyrightText: Copyright Preetham Gujjula
-- SPDX-License-Identifier: BSD-3-Clause
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module VCard.Types.Value.Time
  ( Hour (..),
    HasHour (..),
    Minute (..),
    HasMinute (..),
    Second (..),
    HasSecond (..),
    HourMinuteSecond (..),
    HourMinute (..),
    MinuteSecond (..),
    Sign (..),
    Zone (..),
  )
where

import Control.Applicative (liftA3)
import Data.Char (ord)
import Data.Finite (Finite, getFinite, packFinite)
import Data.Functor (($>))
import Data.Text qualified as Text
import Text.Megaparsec (choice, optional)
import Text.Megaparsec.Char (char, digitChar)
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
-- HasHour
--

-- | Class for types that contain an `Hour`.
class HasHour a where
  getHour :: a -> Hour

instance HasHour Hour where
  getHour = id

instance HasHour HourMinuteSecond where
  getHour (HourMinuteSecond h _ _) = h

instance HasHour HourMinute where
  getHour (HourMinute h _) = h

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
-- HasMinute
--

-- | Class for types that contain a `Minute`.
class HasMinute a where
  getMinute :: a -> Minute

instance HasMinute Minute where
  getMinute = id

instance HasMinute HourMinuteSecond where
  getMinute (HourMinuteSecond _ m _) = m

instance HasMinute HourMinute where
  getMinute (HourMinute _ m) = m

--
-- Second
--

-- | A second of an minute, usually between 00 and 59, and 60 for leap seconds.
newtype Second = Second {unSecond :: Finite 61}
  deriving (Eq, Show, Ord, Bounded)

instance HasSecond HourMinuteSecond where
  getSecond (HourMinuteSecond _ _ s) = s

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

--
-- HasSecond
--

-- | Class for types that contain a `Second`.
class HasSecond a where
  getSecond :: a -> Second

instance HasSecond Second where
  getSecond = id

--
-- HourMinuteSecond
--

-- | An `Hour`, `Minute`, and `Second` together.
data HourMinuteSecond = HourMinuteSecond !Hour !Minute !Second
  deriving (Eq, Show, Ord, Bounded)

--
-- HourMinute
--

-- | An `Hour` and `Minute` together.
data HourMinute = HourMinute !Hour !Minute
  deriving (Eq, Show, Ord, Bounded)

--
-- MinuteSecond
--

-- | A `Minute` and `Second` together.
data MinuteSecond = MinuteSecond !Minute !Second
  deriving (Eq, Show, Ord, Bounded)

--
-- Sign
--
data Sign = Minus | Plus
  deriving (Eq, Show, Ord, Bounded)

instance HasParser Sign where
  parser :: Parser Sign
  parser =
    choice
      [ char '+' $> Plus,
        char '-' $> Minus
      ]

instance HasSerializer Sign where
  serializer :: Serializer Sign
  serializer = \case
    Plus -> "+"
    Minus -> "-"

--
-- Zone
--
data Zone
  = UTCDesignator
  | UTCOffset !Sign !Hour !(Maybe Minute)
  deriving (Eq, Show, Ord)

instance HasParser Zone where
  parser :: Parser Zone
  parser =
    choice
      [ char 'Z' $> UTCDesignator,
        liftA3
          UTCOffset
          (parser @Sign)
          (parser @Hour)
          (optional (parser @Minute))
      ]

instance HasSerializer Zone where
  serializer :: Serializer Zone
  serializer = \case
    UTCDesignator -> "Z"
    UTCOffset sign hour maybeMinute ->
      serializer sign <> serializer hour <> maybe "" serializer maybeMinute

-- Utilities
toDigit :: Char -> Int
toDigit c = ord c - ord '0'
