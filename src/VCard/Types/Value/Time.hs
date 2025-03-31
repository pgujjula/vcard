-- SPDX-FileCopyrightText: Copyright Preetham Gujjula
-- SPDX-License-Identifier: BSD-3-Clause
{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}

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
    LocalTime (..),
    LocalTimeNoTrunc (..),
    LocalTimeComplete (..),
    Time (..),
    TimeList,
    TimeNoTrunc (..),
    TimeComplete (..),
    DateTime (..),
    DateTimeList,
    DateAndOrTime (..),
    DateAndOrTimeList,
    Timestamp (..),
    TimestampList,
    Sign (..),
    UTCDesignator (..),
    UTCOffset (..),
    Zone (..),
  )
where

#if !MIN_VERSION_base(4,18,0)
import Control.Applicative (liftA2, liftA3)
#else
import Control.Applicative (liftA3)
#endif
import Control.Monad (void)
import Data.Char (ord)
import Data.Finite (Finite, getFinite, packFinite)
import Data.Function ((&))
import Data.Functor (($>))
import Data.Text qualified as Text
import Text.Megaparsec (choice, optional, try)
import Text.Megaparsec.Char (char, digitChar, string)
import VCard.Parse (HasParser, Parser, parser)
import VCard.Serialize (HasSerializer, Serializer, serializer)
import VCard.Types.Value.Date (Date, DateComplete, DateNoReduc)
import VCard.Types.Value.List (List)
import Vary (Vary, exhaustiveCase, from, on)

--
-- Hour
--

-- | An hour of the day, between 0 and 23.
--
--   /Reference:/ [@hour@]
--     (https://gist.github.com/pgujjula/af9bacba47664a58eea383a5ae44b10b#file-rfc6350-txt-L549)
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
--
--   /Reference:/ [@minute@]
--     (https://gist.github.com/pgujjula/af9bacba47664a58eea383a5ae44b10b#file-rfc6350-txt-L550)
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
--
--   /Reference:/ [@second@]
--     (https://gist.github.com/pgujjula/af9bacba47664a58eea383a5ae44b10b#file-rfc6350-txt-L551)
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
-- LocalTime
--

-- | The local part of a @time@, without the @zone@.
--
--   /Reference:/ [@time@]
--     (https://gist.github.com/pgujjula/af9bacba47664a58eea383a5ae44b10b#file-rfc6350-txt-L564-L566)
newtype LocalTime = LocalTime
  { unLocalTime ::
      Vary '[Hour, Minute, Second, HourMinuteSecond, HourMinute, MinuteSecond]
  }
  deriving (Eq, Show, Ord)

instance HasParser LocalTime where
  parser =
    LocalTime
      <$> (choice . map try)
        -- The order of these parsers matters. For example, we can't have
        -- hourTimeP before hourMinuteSecondTimeP, because if the string is
        -- something like "081739", then hourTimeP will match the beginning
        -- "08" and parsing will fail on the remainder "1739". So we have to
        -- place hourTimeP after hourMinuteSecondTimeP.
        [ from <$> hourMinuteSecondTimeP,
          from <$> hourMinuteTimeP,
          from <$> minuteSecondTimeP,
          from <$> hourTimeP,
          from <$> minuteTimeP,
          from <$> secondTimeP
        ]

instance HasSerializer LocalTime where
  serializer (LocalTime vary) =
    vary
      & ( on @Hour hourTimeS
            . on @Minute minuteTimeS
            . on @Second secondTimeS
            . on @HourMinuteSecond hourMinuteSecondTimeS
            . on @HourMinute hourMinuteTimeS
            . on @MinuteSecond minuteSecondTimeS
            $ exhaustiveCase
        )

--
-- LocalTimeNoTrunc
--

-- | The local part of a @time-notrunc@, without the @zone@.
--
--   /Reference:/ [@time-notrunc@]
--     (https://gist.github.com/pgujjula/af9bacba47664a58eea383a5ae44b10b#file-rfc6350-txt-L567)
newtype LocalTimeNoTrunc = LocalTimeNoTrunc
  { unLocalTimeNoTrunc ::
      Vary '[Hour, HourMinute, HourMinuteSecond]
  }
  deriving (Eq, Show, Ord)

instance HasParser LocalTimeNoTrunc where
  parser =
    LocalTimeNoTrunc
      <$> (choice . map try)
        [ from <$> hourMinuteSecondTimeP,
          from <$> hourMinuteTimeP,
          from <$> hourTimeP
        ]

instance HasSerializer LocalTimeNoTrunc where
  serializer (LocalTimeNoTrunc vary) =
    vary
      & ( on @Hour hourTimeS
            . on @HourMinute hourMinuteTimeS
            . on @HourMinuteSecond hourMinuteSecondTimeS
            $ exhaustiveCase
        )

--
-- LocalTimeComplete
--

-- | The local part of a @time-complete@, without the @zone@.
--
--   /Reference:/ [@time-complete@]
--     (https://gist.github.com/pgujjula/af9bacba47664a58eea383a5ae44b10b#file-rfc6350-txt-L568)
newtype LocalTimeComplete = LocalTimeComplete
  { unLocalTimeComplete :: HourMinuteSecond
  }
  deriving (Eq, Show, Ord)

instance HasParser LocalTimeComplete where
  parser = LocalTimeComplete <$> hourMinuteSecondTimeP

instance HasSerializer LocalTimeComplete where
  serializer = hourMinuteSecondTimeS . unLocalTimeComplete

--
-- Parsers of various time formats
--

-- Parses "hh"
hourTimeP :: Parser Hour
hourTimeP = parser @Hour

-- Parses "-mm"
minuteTimeP :: Parser Minute
minuteTimeP = char '-' *> parser @Minute

-- Parses "--ss"
secondTimeP :: Parser Second
secondTimeP = string "--" *> parser @Second

-- Parses "hhmmss"
hourMinuteSecondTimeP :: Parser HourMinuteSecond
hourMinuteSecondTimeP =
  liftA3 HourMinuteSecond (parser @Hour) (parser @Minute) (parser @Second)

-- Parses "hhmm"
hourMinuteTimeP :: Parser HourMinute
hourMinuteTimeP = liftA2 HourMinute (parser @Hour) (parser @Minute)

-- Parses "-mmss"
minuteSecondTimeP :: Parser MinuteSecond
minuteSecondTimeP =
  char '-' *> liftA2 MinuteSecond (parser @Minute) (parser @Second)

--
-- Serializers of various time formats
--

-- Serializes to "hh"
hourTimeS :: Serializer Hour
hourTimeS = serializer @Hour

-- Serializes to "-mm"
minuteTimeS :: Serializer Minute
minuteTimeS = ("-" <>) . serializer @Minute

-- Serializes to "--ss"
secondTimeS :: Serializer Second
secondTimeS = ("--" <>) . serializer @Second

-- Serializes to "hhmmss"
hourMinuteSecondTimeS :: Serializer HourMinuteSecond
hourMinuteSecondTimeS (HourMinuteSecond hour minute second) =
  serializer hour <> serializer minute <> serializer second

-- Serializes to "hhmm"
hourMinuteTimeS :: Serializer HourMinute
hourMinuteTimeS (HourMinute hour minute) =
  serializer hour <> serializer minute

-- Serializes to "-mmss"
minuteSecondTimeS :: Serializer MinuteSecond
minuteSecondTimeS (MinuteSecond minute second) =
  "-" <> serializer minute <> serializer second

--
-- Time
--

-- | /Reference:/ [@time@]
--     (https://gist.github.com/pgujjula/af9bacba47664a58eea383a5ae44b10b#file-rfc6350-txt-L564-L566)
data Time = Time
  { timeLocalTime :: !LocalTime,
    timeZone :: !(Maybe Zone)
  }
  deriving (Eq, Show, Ord)

-- | /Reference:/ [@time-list@]
--     (https://gist.github.com/pgujjula/af9bacba47664a58eea383a5ae44b10b#file-rfc6350-txt-L533)
type TimeList = List Time

instance HasParser Time where
  parser :: Parser Time
  parser = liftA2 Time (parser @LocalTime) (optional (parser @Zone))

instance HasSerializer Time where
  serializer :: Serializer Time
  serializer (Time localTime zone) =
    serializer localTime <> maybe "" serializer zone

--
-- TimeNoTrunc
--

-- | /Reference:/ [@time-notrunc@]
--     (https://gist.github.com/pgujjula/af9bacba47664a58eea383a5ae44b10b#file-rfc6350-txt-L567)
data TimeNoTrunc = TimeNoTrunc
  { timeNoTruncLocalTimeNoTrunc :: !LocalTimeNoTrunc,
    timeNoTruncZone :: !(Maybe Zone)
  }
  deriving (Eq, Show, Ord)

instance HasParser TimeNoTrunc where
  parser :: Parser TimeNoTrunc
  parser = liftA2 TimeNoTrunc (parser @LocalTimeNoTrunc) (optional (parser @Zone))

instance HasSerializer TimeNoTrunc where
  serializer :: Serializer TimeNoTrunc
  serializer (TimeNoTrunc localTimeNoTrunc zone) =
    serializer localTimeNoTrunc <> maybe "" serializer zone

--
-- TimeComplete
--

-- | /Reference:/ [@time-complete@]
--     (https://gist.github.com/pgujjula/af9bacba47664a58eea383a5ae44b10b#file-rfc6350-txt-L568)
data TimeComplete = TimeComplete
  { timeCompleteLocalTimeComplete :: !LocalTimeComplete,
    timeCompleteZone :: !(Maybe Zone)
  }
  deriving (Eq, Show, Ord)

instance HasParser TimeComplete where
  parser :: Parser TimeComplete
  parser = liftA2 TimeComplete (parser @LocalTimeComplete) (optional (parser @Zone))

instance HasSerializer TimeComplete where
  serializer :: Serializer TimeComplete
  serializer (TimeComplete localTimeComplete zone) =
    serializer localTimeComplete <> maybe "" serializer zone

--
-- Sign
--

-- | /Reference:/ [@sign@]
--     (https://gist.github.com/pgujjula/af9bacba47664a58eea383a5ae44b10b#file-rfc6350-txt-L544)
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
-- UTCDesignator
--

-- | /Reference:/ [@utc-designator@]
--     (https://gist.github.com/pgujjula/af9bacba47664a58eea383a5ae44b10b#file-rfc6350-txt-L553)
data UTCDesignator = UTCDesignator
  deriving (Eq, Show, Ord)

instance HasParser UTCDesignator where
  parser :: Parser UTCDesignator
  parser = char 'Z' $> UTCDesignator

instance HasSerializer UTCDesignator where
  serializer :: Serializer UTCDesignator
  serializer = const "Z"

--
-- UTCOffset
--

-- | /Reference:/ [@utc-offset@]
--     (https://gist.github.com/pgujjula/af9bacba47664a58eea383a5ae44b10b#file-rfc6350-txt-L583)
data UTCOffset = UTCOffset !Sign !Hour !(Maybe Minute)
  deriving (Eq, Show, Ord)

instance HasParser UTCOffset where
  parser :: Parser UTCOffset
  parser =
    liftA3
      UTCOffset
      (parser @Sign)
      (parser @Hour)
      (optional (parser @Minute))

instance HasSerializer UTCOffset where
  serializer :: Serializer UTCOffset
  serializer (UTCOffset sign hour maybeMinute) =
    serializer sign
      <> serializer hour
      <> maybe "" serializer maybeMinute

--
-- Zone
--

-- | /Reference:/ [@zone@]
--     (https://gist.github.com/pgujjula/af9bacba47664a58eea383a5ae44b10b#file-rfc6350-txt-L552)
newtype Zone = Zone {unZone :: Vary '[UTCDesignator, UTCOffset]}
  deriving (Eq, Show, Ord)

instance HasParser Zone where
  parser :: Parser Zone
  parser =
    Zone
      <$> choice
        [ Vary.from <$> parser @UTCDesignator,
          Vary.from <$> parser @UTCOffset
        ]

instance HasSerializer Zone where
  serializer :: Serializer Zone
  serializer =
    ( on @UTCDesignator serializer
        . on @UTCOffset serializer
        $ exhaustiveCase
    )
      . unZone

--
-- DateTime
--

-- | /Reference:/ [@date-time@]
--     (https://gist.github.com/pgujjula/af9bacba47664a58eea383a5ae44b10b#file-rfc6350-txt-L578)
data DateTime = DateTime
  { dateTimeDateNoReduc :: DateNoReduc,
    dateTimeTimeNoTrunc :: TimeNoTrunc
  }
  deriving (Eq, Show, Ord)

instance HasParser DateTime where
  parser :: Parser DateTime
  parser = do
    dateNoReduc <- parser @DateNoReduc
    void (char 'T')
    timeNoTrunc <- parser @TimeNoTrunc
    pure $
      DateTime
        { dateTimeDateNoReduc = dateNoReduc,
          dateTimeTimeNoTrunc = timeNoTrunc
        }

instance HasSerializer DateTime where
  serializer :: Serializer DateTime
  serializer dateTime =
    serializer (dateTimeDateNoReduc dateTime)
      <> "T"
      <> serializer (dateTimeTimeNoTrunc dateTime)

-- | /Reference:/ [@date-time-list@]
--     (https://gist.github.com/pgujjula/af9bacba47664a58eea383a5ae44b10b#file-rfc6350-txt-L534)
type DateTimeList = List DateTime

--
-- DateAndOrTime
--

-- | /Reference:/ [@date-and-or-time@]
--     (https://gist.github.com/pgujjula/af9bacba47664a58eea383a5ae44b10b#file-rfc6350-txt-L581)
newtype DateAndOrTime = DateAndOrTime
  { unDateAndOrTime :: Vary '[DateTime, Date, Time]
  }
  deriving (Eq, Show, Ord)

instance HasParser DateAndOrTime where
  parser :: Parser DateAndOrTime
  parser =
    choice $
      map
        (try . fmap DateAndOrTime)
        [ fmap from (parser @DateTime),
          fmap from (parser @Date),
          fmap from (char 'T' *> parser @Time)
        ]

instance HasSerializer DateAndOrTime where
  serializer :: Serializer DateAndOrTime
  serializer (DateAndOrTime vary) =
    vary
      & ( on @DateTime (serializer @DateTime)
            . on @Date (serializer @Date)
            . on @Time (\x -> "T" <> serializer @Time x)
            $ exhaustiveCase
        )

-- | /Reference:/ [@date-and-or-time-list@]
--     (https://gist.github.com/pgujjula/af9bacba47664a58eea383a5ae44b10b#file-rfc6350-txt-L535)
type DateAndOrTimeList = List DateAndOrTime

--
-- Timestamp
--

-- | /Reference:/ [@timestamp@]
--     (https://gist.github.com/pgujjula/af9bacba47664a58eea383a5ae44b10b#file-rfc6350-txt-L579)
data Timestamp = Timestamp
  { timestampDateComplete :: DateComplete,
    timestampTimeComplete :: TimeComplete
  }
  deriving (Eq, Show, Ord)

instance HasParser Timestamp where
  parser :: Parser Timestamp
  parser = do
    dateComplete <- parser @DateComplete
    void (char 'T')
    timeComplete <- parser @TimeComplete
    pure $
      Timestamp
        { timestampDateComplete = dateComplete,
          timestampTimeComplete = timeComplete
        }

instance HasSerializer Timestamp where
  serializer :: Serializer Timestamp
  serializer timestamp =
    serializer (timestampDateComplete timestamp)
      <> "T"
      <> serializer (timestampTimeComplete timestamp)

-- | /Reference:/ [@timestamp-list@]
--     (https://gist.github.com/pgujjula/af9bacba47664a58eea383a5ae44b10b#file-rfc6350-txt-L536)
type TimestampList = List Timestamp

-- Utilities
toDigit :: Char -> Int
toDigit c = ord c - ord '0'
