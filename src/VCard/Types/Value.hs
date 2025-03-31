-- SPDX-FileCopyrightText: Copyright Preetham Gujjula
-- SPDX-License-Identifier: BSD-3-Clause

-- |
-- Module     : VCard.Types.Value
-- Copyright  : Copyright Preetham Gujjula
-- License    : BSD-3-Clause
-- Maintainer : Preetham Gujjula <libraries@mail.preetham.io>
-- Stability  : experimental
--
-- Property value data types.
--
-- /Reference:/ [/RFC 6350 Section 4/]
--    (https://gist.github.com/pgujjula/af9bacba47664a58eea383a5ae44b10b#file-rfc6350-txt-L494)
module VCard.Types.Value
  ( -- * Section 4.1 @TEXT@

    -- | /Reference:/ [/RFC 6350 Section 4.1/]
    --     (https://gist.github.com/pgujjula/af9bacba47664a58eea383a5ae44b10b#file-rfc6350-txt-L592)
    Text,
    TextList,

    -- * Section 4.2 @URI@

    -- | The 'URI' type here is re-exported from "Network.URI". This works well
    --   because Network.URI implements RFC 3986, which is also the basis of the
    --   URI type in the vCard spec.
    --
    --   /Reference:/ [/RFC 6350 Section 4.2/]
    --     (https://gist.github.com/pgujjula/af9bacba47664a58eea383a5ae44b10b#file-rfc6350-txt-L633)
    URI (..),

    -- * Section 4.3 @DATE@, @TIME@, @DATE-TIME@, @DATE-AND-OR-TIME@, and @TIMESTAMP@

    -- | /Reference:/ [/RFC 6350 Section 4.3/]
    --     (https://gist.github.com/pgujjula/af9bacba47664a58eea383a5ae44b10b#file-rfc6350-txt-L648)

    -- ** Section 4.3.1 @DATE@

    -- | /Reference:/ [/RFC 6350 Section 4.3.1/]
    --     (https://gist.github.com/pgujjula/af9bacba47664a58eea383a5ae44b10b#file-rfc6350-txt-L657)
    Date (..),
    DateList,

    -- ** Section 4.3.2 @TIME@

    -- | /Reference:/ [/RFC 6350 Section 4.3.2/]
    --     (https://gist.github.com/pgujjula/af9bacba47664a58eea383a5ae44b10b#file-rfc6350-txt-L694)
    Time (..),
    TimeList,
    LocalTime (..),

    -- ** Section 4.3.3 @DATE-TIME@

    -- | /Reference:/ [/RFC 6350 Section 4.3.3/]
    --     (https://gist.github.com/pgujjula/af9bacba47664a58eea383a5ae44b10b#file-rfc6350-txt-L720)
    DateTime (..),
    DateTimeList,
    DateNoReduc (..),
    TimeNoTrunc (..),
    LocalTimeNoTrunc (..),

    -- ** Section 4.3.4 @DATE-AND-OR-TIME@

    -- | /Reference:/ [/RFC 6350 Section 4.3.4/]
    --     (https://gist.github.com/pgujjula/af9bacba47664a58eea383a5ae44b10b#file-rfc6350-txt-L745)
    DateAndOrTime (..),
    DateAndOrTimeList,

    -- ** Section 4.3.5 @TIMESTAMP@

    -- | /Reference:/ [/RFC 6350 Section 4.3.5/]
    --     (https://gist.github.com/pgujjula/af9bacba47664a58eea383a5ae44b10b#file-rfc6350-txt-L768)
    Timestamp (..),
    TimestampList,
    DateComplete (..),
    TimeComplete (..),
    LocalTimeComplete (..),

    -- ** Date components
    Year (..),
    HasYear (..),
    Month (..),
    HasMonth (..),
    Day (..),
    HasDay (..),
    YearMonthDay,
    mkYearMonthDay,
    YearMonth (..),
    MonthDay,
    mkMonthDay,

    -- ** Time components
    Hour (..),
    HasHour (..),
    Minute (..),
    HasMinute (..),
    Second (..),
    HasSecond (..),
    HourMinuteSecond (..),
    HourMinute (..),
    MinuteSecond (..),
    Zone (..),
    UTCDesignator (..),

    -- * Section 4.4 @BOOLEAN@

    -- | /Reference:/ [/RFC 6350 Section 4.4/]
    --     (https://gist.github.com/pgujjula/af9bacba47664a58eea383a5ae44b10b#file-rfc6350-txt-L780)
    Boolean (..),

    -- * Section 4.5 @INTEGER@

    -- | /Reference:/ [/RFC 6350 Section 4.5/]
    --     (https://gist.github.com/pgujjula/af9bacba47664a58eea383a5ae44b10b#file-rfc6350-txt-L801)
    Integer (..),
    IntegerList,
    IntegerValue (..),

    -- * Section 4.6 @FLOAT@

    -- | /Reference:/ [/RFC 6350 Section 4.6/]
    --     (https://gist.github.com/pgujjula/af9bacba47664a58eea383a5ae44b10b#file-rfc6350-txt-L817)
    Float (..),
    FloatList,
    NaturalLeadingZeros (..),
    toScientific,
    fromScientific,

    -- * Section 4.7 @UTC-OFFSET@

    -- | /Reference:/ [/RFC 6350 Section 4.7/]
    --     (https://gist.github.com/pgujjula/af9bacba47664a58eea383a5ae44b10b#file-rfc6350-txt-L834)
    UTCOffset (..),

    -- * Section 4.8 @LANGUAGE-TAG@

    -- | /Reference:/ [/RFC 6350 Section 4.8/]
    --     (https://gist.github.com/pgujjula/af9bacba47664a58eea383a5ae44b10b#file-rfc6350-txt-L857)
    LanguageTag (..),

    -- * Generic @List@ type
    List (..),

    -- * Miscellaneous
    Sign (..),
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
import VCard.Types.Value.Float
  ( Float (..),
    FloatList,
    NaturalLeadingZeros (..),
    fromScientific,
    toScientific,
  )
import VCard.Types.Value.Integer (Integer (..), IntegerList, IntegerValue (..))
import VCard.Types.Value.LanguageTag (LanguageTag (..))
import VCard.Types.Value.List (List (..))
import VCard.Types.Value.Text (Text (..), TextList)
import VCard.Types.Value.Time
  ( DateAndOrTime (..),
    DateAndOrTimeList,
    DateTime (..),
    DateTimeList,
    HasHour (..),
    HasMinute (..),
    HasSecond (..),
    Hour (..),
    HourMinute (..),
    HourMinuteSecond (..),
    LocalTime (..),
    LocalTimeComplete (..),
    LocalTimeNoTrunc (..),
    Minute (..),
    MinuteSecond (..),
    Second (..),
    Sign (..),
    Time (..),
    TimeComplete (..),
    TimeList,
    TimeNoTrunc (..),
    Timestamp (..),
    TimestampList,
    UTCDesignator (..),
    UTCOffset (..),
    Zone (..),
  )
import VCard.Types.Value.URI (URI (..))
import Prelude hiding (Float, Integer)
