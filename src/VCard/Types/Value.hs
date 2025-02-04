-- SPDX-FileCopyrightText: Copyright Preetham Gujjula
-- SPDX-License-Identifier: BSD-3-Clause

-- |
-- Module     : VCard.Types.Value
-- Copyright  : Copyright Preetham Gujjula
-- License    : BSD-3-Clause
-- Maintainer : Preetham Gujjula <libraries@mail.preetham.io>
-- Stability  : experimental
--
-- Types for the property value data types described in Section 4 of RFC 6350.
module VCard.Types.Value
  ( -- * Section 4.1 @TEXT@
    Text,
    TextList,

    -- * Section 4.2 @URI@

    -- | The 'URI' type here is re-exported from "Network.URI". This works well
    -- because Network.URI implements RFC 3986, which is also the basis of the
    -- URI type in the vCard spec.
    URI (..),
    List,

    -- * Section 4.3.1 @DATE@

    -- ** 'Date'
    Date (..),
    DateList,

    -- ** 'DateNoReduc'
    DateNoReduc (..),

    -- ** 'DateComplete
    DateComplete (..),

    -- ** 'Year'
    Year (..),
    HasYear (..),

    -- ** 'Month'
    Month (..),
    HasMonth (..),

    -- ** 'Day'
    Day (..),
    HasDay (..),

    -- ** 'YearMonthDay'
    YearMonthDay,
    mkYearMonthDay,

    -- ** 'YearMonth'
    YearMonth (..),

    -- ** 'MonthDay'
    MonthDay,
    mkMonthDay,

    -- * Section 4.3.2 @TIME@

    -- ** 'Time'
    LocalTime (..),

    -- ** 'Hour'
    Hour (..),
    HasHour (..),

    -- ** 'Minute'
    Minute (..),
    HasMinute (..),

    -- ** 'Second'
    Second (..),
    HasSecond (..),

    -- ** 'HourMinuteSecond'
    HourMinuteSecond (..),

    -- ** 'HourMinute'
    HourMinute (..),

    -- ** 'MinuteSecond'
    MinuteSecond (..),

    -- ** 'Zone'
    Zone (..),

    -- ** 'TimeNoTrunc'
    TimeNoTrunc (..),

    -- ** 'TimeComplete'
    TimeComplete (..),

    -- ** 'DateTime'
    DateTime (..),
    DateTimeList,

    -- ** 'Timestamp'
    Timestamp (..),
    TimestampList,

    -- * Section 4.4 @BOOLEAN@
    Boolean (..),

    -- * Section 4.5 @INTEGER@
    Integer (..),
    IntegerList,

    -- * Section 4.7 @UTC-OFFSET@
    UTCOffset (..),

    -- * Section 4.7 @LANGUAGE-TAG@
    LanguageTag (..),
  )
where

import VCard.Types.Value.Boolean (Boolean (..))
import VCard.Types.Value.Date
  ( Date (..),
    DateComplete (..),
    DateList,
    DateNoReduc (..),
    Day (..),
    HasDay (..),
    HasMonth (..),
    HasYear (..),
    Month (..),
    MonthDay,
    Year (..),
    YearMonth (..),
    YearMonthDay (..),
    mkMonthDay,
    mkYearMonthDay,
  )
import VCard.Types.Value.Integer (Integer (..), IntegerList)
import VCard.Types.Value.LanguageTag (LanguageTag (..))
import VCard.Types.Value.List (List (..))
import VCard.Types.Value.Text (Text (..), TextList)
import VCard.Types.Value.Time
  ( DateTime (..),
    DateTimeList,
    HasHour (..),
    HasMinute (..),
    HasSecond (..),
    Hour (..),
    HourMinute (..),
    HourMinuteSecond (..),
    LocalTime (..),
    Minute (..),
    MinuteSecond (..),
    Second (..),
    TimeComplete (..),
    TimeNoTrunc (..),
    Timestamp (..),
    TimestampList,
    UTCOffset (..),
    Zone (..),
  )
import VCard.Types.Value.URI (URI (..))
import Prelude hiding (Integer)
