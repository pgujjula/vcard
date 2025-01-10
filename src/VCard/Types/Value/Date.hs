-- SPDX-FileCopyrightText: Copyright Preetham Gujjula
-- SPDX-License-Identifier: BSD-3-Clause
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiWayIf #-}

module VCard.Types.Value.Date
  ( Year (..),
    HasYear (..),
    Month (..),
    HasMonth (..),
    Day (..),
    HasDay (..),
    YearMonthDay (..),
    mkYearMonthDay,
    YearMonth (..),
    MonthDay (..),
    mkMonthDay,
  )
where

import Data.Char (ord)
import Data.Finite (Finite, getFinite, packFinite)
import Data.Text qualified as Text
import Text.Megaparsec.Char (digitChar)
import TextShow (showt)
import VCard.Parse (HasParser, Parser, parser)
import VCard.Serialize (HasSerializer, Serializer, serializer)

--
-- Year
--

-- | A year between 0000 and 9999. @'Year' ('Data.Finite.finite' 0)@ is year
-- 0000 and @'Year' ('Data.Finite.finite' 9999)@ is year 9999.
newtype Year = Year {unYear :: Finite 10000}
  deriving (Eq, Show, Ord, Bounded)

instance HasParser Year where
  parser :: Parser Year
  parser = do
    d1 <- toDigit <$> digitChar
    d2 <- toDigit <$> digitChar
    d3 <- toDigit <$> digitChar
    d4 <- toDigit <$> digitChar
    let yearInt = toInteger (1000 * d1 + 100 * d2 + 10 * d3 + d4)
    case packFinite yearInt of
      Just yearFinite -> pure (Year yearFinite)
      Nothing ->
        fail $
          show yearInt
            <> " was out of bounds for year (0000 to 9999)."
            <> " This should be impossible"

instance HasSerializer Year where
  serializer :: Serializer Year
  serializer (Year year) =
    let yearInt = getFinite year
        (d1, r1) = yearInt `quotRem` 1000
        (d2, r2) = r1 `quotRem` 100
        (d3, d4) = r2 `quotRem` 10
     in Text.concat (map showt [d1, d2, d3, d4])

--
-- Month
--

-- | A month of the year. @'Month' ('Data.Finite.finite' 0)@ is January and
-- @'Month' ('Data.Finite.finite' 11)@ is December.
newtype Month = Month {unMonth :: Finite 12}
  deriving (Eq, Show, Ord, Bounded)

instance HasParser Month where
  parser :: Parser Month
  parser = do
    d1 <- toDigit <$> digitChar
    d2 <- toDigit <$> digitChar
    let monthInt = 10 * d1 + d2
    case packFinite (toInteger monthInt - 1) of
      Just monthFinite -> pure (Month monthFinite)
      Nothing ->
        fail $ show monthInt <> " was out of bounds for month (01 to 12)"

instance HasSerializer Month where
  serializer :: Serializer Month
  serializer (Month month) =
    let monthInt = getFinite month + 1
        (d1, d2) = monthInt `quotRem` 10
     in showt d1 <> showt d2

--
-- Day
--

-- | A day of the month. @'Day' ('Data.Finite.finite' 0)@ is the 1st of the month
--   and @'Day' ('Data.Finite.finite' 30)@ is the 31st.
newtype Day = Day {unDay :: Finite 31}
  deriving (Eq, Show, Ord, Bounded)

instance HasParser Day where
  parser :: Parser Day
  parser = do
    d1 <- toDigit <$> digitChar
    d2 <- toDigit <$> digitChar
    let dayInt = 10 * d1 + d2
    case packFinite (toInteger dayInt - 1) of
      Just dayFinite -> pure (Day dayFinite)
      Nothing -> fail $ show dayInt <> " was out of bounds for day (01 to 31)"

instance HasSerializer Day where
  serializer :: Serializer Day
  serializer (Day day) =
    let dayInt = getFinite day + 1
        (d1, d2) = dayInt `quotRem` 10
     in showt d1 <> showt d2

--
-- YearMonthDay
--

-- | A 'Year', 'Month', and 'Day' together.
data YearMonthDay = YearMonthDay !Year !Month !Day
  deriving (Eq, Show, Ord, Bounded)

-- | Create a 'YearMonthDay'. Yields 'Nothing' if the 'YearMonthDay' would be
--   invalid.
--
-- @
-- -- Create January 15, 1970
-- >>> let year = Year (finite 1970)
-- >>> let month = Month (finite 0)
-- >>> let day = Day (finite 14)
-- >>> isJust (mkYearMonthDay year month day)
-- True
--
-- -- Create February 29, 1970 (not a leap year, doesn't exist)
-- >>> let year = Year (finite 1970)
-- >>> let month = Month (finite 1)
-- >>> let day = Day (finite 28)
-- >>> isJust (mkYearMonthDay year month day)
-- False
--
-- -- Create February 29, 1972 (leap year, exists)
-- >>> let year = Year (finite 1972)
-- >>> let month = Month (finite 1)
-- >>> let day = Day (finite 28)
-- >>> isJust (mkYearMonthDay year month day)
-- True
-- @
mkYearMonthDay :: Year -> Month -> Day -> Maybe YearMonthDay
mkYearMonthDay year month day = do
  monthDay <- mkMonthDay month day
  if isFeb29 monthDay && not (isLeapYear year)
    then Nothing
    else Just (YearMonthDay year month day)

isFeb29 :: MonthDay -> Bool
isFeb29 (MonthDay month day) =
  getFinite (unMonth month) == 1
    && getFinite (unDay day) == 28

isLeapYear :: Year -> Bool
isLeapYear year =
  let y = getFinite (unYear year)
   in if
        | y `rem` 400 == 0 -> True
        | y `rem` 100 == 0 -> False
        | y `rem` 4 == 0 -> True
        | otherwise -> False

-- | A 'Year' and 'Month' together. For example,
--
-- @
-- 'YearMonth' ('Year' ('Data.Finite.finite' 1970)) ('Month' ('Data.Finite.finite' 0))
-- @
--
-- represents January 1970.
data YearMonth = YearMonth !Year !Month
  deriving (Eq, Show, Ord, Bounded)

--
-- MonthDay
--

data MonthDay = MonthDay !Month !Day
  deriving (Eq, Show, Ord, Bounded)

-- | Create a 'MonthDay'. Yields 'Nothing' if the 'MonthDay' would be invalid.
--
-- @
-- -- Create January 15
-- >>> isJust (mkMonthDay (Month (finite 0)) (Day (finite 14)))
-- True
--
-- -- Create February 29
-- >>> isJust (mkMonthDay (Month (finite 1)) (Day (finite 28)))
-- True
--
-- -- Create April 31 (doesn't exist)
-- >>> isJust (mkMonthDay (Month (finite 3)) (Day (finite 30)))
-- False
-- @
mkMonthDay :: Month -> Day -> Maybe MonthDay
mkMonthDay month day =
  let -- between 1 and 12
      monthInteger :: Integer
      monthInteger = (+ 1) . getFinite . unMonth $ month

      -- between 1 and 31
      dayInteger :: Integer
      dayInteger = (+ 1) . getFinite . unDay $ day

      maxDay :: Integer
      maxDay =
        case monthInteger of
          1 -> 31
          2 -> 29
          3 -> 31
          4 -> 30
          5 -> 31
          6 -> 30
          7 -> 31
          8 -> 31
          9 -> 30
          10 -> 31
          11 -> 30
          _ -> 31
   in if dayInteger <= maxDay
        then Just (MonthDay month day)
        else Nothing

--
-- HasYear
--

-- | Class for types that contain a 'Year'.
class HasYear a where
  getYear :: a -> Year

instance HasYear Year where
  getYear = id

instance HasYear YearMonthDay where
  getYear (YearMonthDay year _ _) = year

instance HasYear YearMonth where
  getYear (YearMonth year _) = year

--
-- HasMonth
--

-- | Class for types that contain a 'Month'.
class HasMonth a where
  getMonth :: a -> Month

instance HasMonth Month where
  getMonth = id

instance HasMonth YearMonthDay where
  getMonth (YearMonthDay _ month _) = month

instance HasMonth YearMonth where
  getMonth (YearMonth _ month) = month

instance HasMonth MonthDay where
  getMonth (MonthDay month _) = month

--
-- HasDay
--

-- | Class for types that contain a 'Day'.
class HasDay a where
  getDay :: a -> Day

instance HasDay Day where
  getDay = id

instance HasDay YearMonthDay where
  getDay (YearMonthDay _ _ day) = day

instance HasDay MonthDay where
  getDay (MonthDay _ day) = day

-- Utilities
toDigit :: Char -> Int
toDigit c = ord c - ord '0'
