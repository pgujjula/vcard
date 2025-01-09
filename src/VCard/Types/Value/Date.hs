-- SPDX-FileCopyrightText: Copyright Preetham Gujjula
-- SPDX-License-Identifier: BSD-3-Clause
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE InstanceSigs #-}

module VCard.Types.Value.Date
  ( Year (..),
    HasYear (..),
    Month (..),
    HasMonth (..),
    Day (..),
    YearMonth (..),
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
  deriving (Eq, Show, Ord)

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
  deriving (Eq, Show, Ord)

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
  deriving (Eq, Show, Ord)

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
-- YearMonth
--

-- | A 'Year' and 'Month' together. For example,
--
-- @
-- 'YearMonth' ('Year' ('Data.Finite.finite' 1970)) ('Month' ('Data.Finite.finite' 0))
-- @
--
-- represents January 1970.
data YearMonth = YearMonth !Year !Month
  deriving (Eq, Show, Ord)

--
-- HasYear
--

-- | Class for types that contain a 'Year'.
class HasYear a where
  getYear :: a -> Year

instance HasYear Year where
  getYear = id

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

instance HasMonth YearMonth where
  getMonth (YearMonth _ month) = month

-- Utilities
toDigit :: Char -> Int
toDigit c = ord c - ord '0'
