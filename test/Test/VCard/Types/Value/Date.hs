-- SPDX-FileCopyrightText: Copyright Preetham Gujjula
-- SPDX-License-Identifier: BSD-3-Clause
{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Test.VCard.Types.Value.Date (tests) where

#if !MIN_VERSION_base(4,18,0)
import Control.Applicative (liftA2, liftA3)
#else
import Control.Applicative (liftA3)
#endif
import Control.Monad (forM_, replicateM)
import Data.Finite (finite, finites, getFinite)
import Data.Maybe (isJust, isNothing)
import Data.Proxy (Proxy (..))
import Data.Set ((\\))
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Time qualified as Time
import Data.Time.Calendar.MonthDay
  ( DayOfMonth,
    MonthOfYear,
    monthAndDayToDayOfYearValid,
  )
import Data.Time.Calendar.OrdinalDate qualified as Time
import Test.Tasty (TestName, TestTree, testGroup)
import Test.Tasty.ExpectedFailure (ignoreTestBecause)
import Test.Tasty.HUnit (assertBool, assertFailure, testCase, (@?=))
import TextShow (showt)
import VCard.Parse (HasParser, parse)
import VCard.Serialize (HasSerializer, serialize)
import VCard.Types.Value.Date
  ( Day (..),
    Month (..),
    MonthDay (..),
    Year (..),
    YearMonth (..),
    YearMonthDay (..),
    getDay,
    getMonth,
    getYear,
    mkMonthDay,
    mkYearMonthDay,
  )

{-# ANN module ("HLint: ignore Use camelCase" :: String) #-}

tests :: TestTree
tests =
  testGroup
    "Date"
    [ test_Year,
      test_Month,
      test_Day,
      test_YearMonthDay,
      test_YearMonth,
      test_MonthDay
    ]

--
-- Year
--
test_Year :: TestTree
test_Year =
  testGroup
    "Year"
    [ test_Year_parse,
      test_Year_serialize,
      test_Year_bounds
    ]

test_Year_parse :: TestTree
test_Year_parse =
  testGroup
    "parse"
    [ test_Year_parse_unit,
      test_Year_parse_exhaustive
    ]

test_Year_parse_unit :: TestTree
test_Year_parse_unit =
  testGroup
    "unit"
    [ test_Year_parse_unit_valid,
      test_Year_parse_unit_invalid
    ]

test_Year_parse_exhaustive :: TestTree
test_Year_parse_exhaustive =
  testGroup
    "exhaustive"
    [ test_Year_parse_exhaustive_valid,
      test_Year_parse_exhaustive_invalid
    ]

test_Year_parse_unit_valid :: TestTree
test_Year_parse_unit_valid =
  unimplemented "valid"

test_Year_parse_unit_invalid :: TestTree
test_Year_parse_unit_invalid =
  testParseInvalid (Proxy @Year) invalidYears

test_Year_parse_exhaustive_valid :: TestTree
test_Year_parse_exhaustive_valid =
  testParseValid exhaustive_Year_valid

test_Year_parse_exhaustive_invalid :: TestTree
test_Year_parse_exhaustive_invalid =
  unimplemented "invalid"

test_Year_serialize :: TestTree
test_Year_serialize =
  testGroup
    "serialize"
    [ test_Year_serialize_unit,
      test_Year_serialize_exhaustive
    ]

test_Year_serialize_unit :: TestTree
test_Year_serialize_unit = unimplemented "unit"

test_Year_serialize_exhaustive :: TestTree
test_Year_serialize_exhaustive =
  testSerialize "exhaustive" exhaustive_Year_valid

invalidYears :: [Text]
invalidYears =
  [ "1",
    "01",
    "001",
    "20",
    "020",
    "-1",
    "-01",
    "-001",
    "-0001",
    "-20",
    "-020",
    "-0200",
    "10000",
    "20a9",
    "2000\n"
  ]

test_Year_bounds :: TestTree
test_Year_bounds = testBounds (y 0000, y 9999)

--
-- Month
--
test_Month :: TestTree
test_Month =
  testGroup
    "Month"
    [ test_Month_parse,
      test_Month_serialize,
      test_Month_bounds
    ]

test_Month_parse :: TestTree
test_Month_parse =
  testGroup
    "parse"
    [ test_Month_parse_unit,
      test_Month_parse_exhaustive
    ]

test_Month_parse_unit :: TestTree
test_Month_parse_unit =
  testGroup
    "unit"
    [ test_Month_parse_unit_valid,
      test_Month_parse_unit_invalid
    ]

test_Month_parse_exhaustive :: TestTree
test_Month_parse_exhaustive =
  testGroup
    "exhaustive"
    [ test_Month_parse_exhaustive_valid,
      test_Month_parse_exhaustive_invalid
    ]

test_Month_parse_unit_valid :: TestTree
test_Month_parse_unit_valid = unimplemented "valid"

test_Month_parse_unit_invalid :: TestTree
test_Month_parse_unit_invalid = testParseInvalid (Proxy @Month) invalidMonths

test_Month_parse_exhaustive_valid :: TestTree
test_Month_parse_exhaustive_valid = testParseValid exhaustive_Month_valid

test_Month_parse_exhaustive_invalid :: TestTree
test_Month_parse_exhaustive_invalid = unimplemented "invalid"

test_Month_serialize :: TestTree
test_Month_serialize =
  testGroup
    "serialize"
    [ test_Month_serialize_unit,
      test_Month_serialize_exhaustive
    ]

test_Month_serialize_unit :: TestTree
test_Month_serialize_unit = unimplemented "unit"

test_Month_serialize_exhaustive :: TestTree
test_Month_serialize_exhaustive =
  testSerialize "exhaustive" exhaustive_Month_valid

invalidMonths :: [Text]
invalidMonths =
  [ "0",
    "00",
    "000",
    "0000",
    "00\n",
    "1",
    "001",
    "0001",
    "01\n",
    "7",
    "007",
    "0007",
    "07\n",
    "010",
    "0010",
    "10\n",
    "012",
    "0012",
    "12\n",
    "13",
    "013",
    "0013",
    "13\n",
    "20",
    "020",
    "0020",
    "20\n",
    "-1",
    "-01",
    "-001",
    "-0001",
    "-1\n",
    "-01\n",
    "-12",
    "-012",
    "-0012",
    "-12\n",
    "-20",
    "-020",
    "-0020",
    "-20\n",
    "a",
    "1a",
    "a1",
    "a"
  ]

test_Month_bounds :: TestTree
test_Month_bounds = testBounds (m 01, m 12)

--
-- Day
--
test_Day :: TestTree
test_Day =
  testGroup
    "Day"
    [ test_Day_parse,
      test_Day_serialize,
      test_Day_bounds
    ]

test_Day_parse :: TestTree
test_Day_parse =
  testGroup
    "parse"
    [ test_Day_parse_unit,
      test_Day_parse_exhaustive
    ]

test_Day_parse_unit :: TestTree
test_Day_parse_unit =
  testGroup
    "unit"
    [ test_Day_parse_unit_valid,
      test_Day_parse_unit_invalid
    ]

test_Day_parse_exhaustive :: TestTree
test_Day_parse_exhaustive =
  testGroup
    "exhaustive"
    [ test_Day_parse_exhaustive_valid,
      test_Day_parse_exhaustive_invalid
    ]

test_Day_parse_unit_valid :: TestTree
test_Day_parse_unit_valid = unimplemented "valid"

test_Day_parse_unit_invalid :: TestTree
test_Day_parse_unit_invalid = testParseInvalid (Proxy @Day) invalidDays

test_Day_parse_exhaustive_valid :: TestTree
test_Day_parse_exhaustive_valid = testParseValid exhaustive_Day_valid

test_Day_parse_exhaustive_invalid :: TestTree
test_Day_parse_exhaustive_invalid = unimplemented "invalid"

test_Day_serialize :: TestTree
test_Day_serialize =
  testGroup
    "serialize"
    [ test_Day_serialize_unit,
      test_Day_serialize_exhaustive
    ]

test_Day_serialize_unit :: TestTree
test_Day_serialize_unit = unimplemented "unit"

test_Day_serialize_exhaustive :: TestTree
test_Day_serialize_exhaustive = testSerialize "exhaustive" exhaustive_Day_valid

test_Day_bounds :: TestTree
test_Day_bounds = testBounds (d 01, d 31)

invalidDays :: [Text]
invalidDays =
  [ "0",
    "00",
    "000",
    "0000",
    "00\n",
    "1",
    "001",
    "0001",
    "01\n",
    "7",
    "007",
    "0007",
    "07\n",
    "030",
    "0030",
    "30\n",
    "031",
    "0031",
    "31\n",
    "32",
    "032",
    "0032",
    "32\n",
    "40",
    "040",
    "0040",
    "40\n",
    "-1",
    "-01",
    "-001",
    "-0001",
    "-1\n",
    "-01\n",
    "-12",
    "-012",
    "-0012",
    "-12\n",
    "-20",
    "-020",
    "-0020",
    "-20\n",
    "a",
    "1a",
    "a1",
    "a\n"
  ]

--
-- YearMonthDay
--
test_YearMonthDay :: TestTree
test_YearMonthDay =
  testGroup
    "YearMonthDay"
    [ test_YearMonthDay_mkYearMonthDay,
      test_YearMonthDay_bounds
    ]

test_YearMonthDay_mkYearMonthDay :: TestTree
test_YearMonthDay_mkYearMonthDay =
  testGroup
    "mkYearMonthDay"
    [ test_YearMonthDay_mkYearMonthDay_unit,
      test_YearMonthDay_mkYearMonthDay_exhaustive
    ]

test_YearMonthDay_mkYearMonthDay_unit :: TestTree
test_YearMonthDay_mkYearMonthDay_unit =
  testGroup
    "unit"
    [ test_YearMonthDay_mkYearMonthDay_unit_valid,
      test_YearMonthDay_mkYearMonthDay_unit_invalid
    ]

test_YearMonthDay_mkYearMonthDay_exhaustive :: TestTree
test_YearMonthDay_mkYearMonthDay_exhaustive =
  testGroup
    "exhaustive"
    [ test_YearMonthDay_mkYearMonthDay_exhaustive_valid,
      test_YearMonthDay_mkYearMonthDay_exhaustive_invalid
    ]

test_YearMonthDay_mkYearMonthDay_unit_valid :: TestTree
test_YearMonthDay_mkYearMonthDay_unit_valid = unimplemented "valid"

test_YearMonthDay_mkYearMonthDay_unit_invalid :: TestTree
test_YearMonthDay_mkYearMonthDay_unit_invalid = unimplemented "invalid"

test_YearMonthDay_mkYearMonthDay_exhaustive_valid :: TestTree
test_YearMonthDay_mkYearMonthDay_exhaustive_valid =
  testCase "valid" $
    forM_ validYearMonthDays $ \(year, month, day) -> do
      yearMonthDay <-
        case mkYearMonthDay year month day of
          Nothing ->
            assertFailure $
              "could not create YearMonthDay from "
                <> show year
                <> ", "
                <> show month
                <> ", "
                <> show day
          Just x -> pure x
      getYear yearMonthDay @?= year
      getMonth yearMonthDay @?= month
      getDay yearMonthDay @?= day

test_YearMonthDay_mkYearMonthDay_exhaustive_invalid :: TestTree
test_YearMonthDay_mkYearMonthDay_exhaustive_invalid =
  testCase "invalid" $
    forM_ invalidYearMonthDays $ \(year, month, day) ->
      case mkYearMonthDay year month day of
        Nothing -> pure ()
        Just yearMonthDay -> assertFailure $ "made invalid " <> show yearMonthDay

validYearMonthDays :: [(Year, Month, Day)]
validYearMonthDays =
  let startTimeDay :: Time.Day
      startTimeDay = Time.fromOrdinalDate 0 1

      endTimeDay :: Time.Day
      endTimeDay = Time.fromOrdinalDate 9999 365

      timeDays :: [Time.Day]
      timeDays = [startTimeDay .. endTimeDay]
   in map timeDayToYearMonthDay timeDays

timeDayToYearMonthDay :: Time.Day -> (Year, Month, Day)
timeDayToYearMonthDay timeDay =
  let timeYear :: Time.Year
      timeMonthOfYear :: Time.MonthOfYear
      timeDayOfMonth :: Time.DayOfMonth
      (timeYear, timeMonthOfYear, timeDayOfMonth) = Time.toGregorian timeDay

      year :: Year
      year = y timeYear

      month :: Month
      month = m (toInteger timeMonthOfYear)

      day :: Day
      day = d (toInteger timeDayOfMonth)
   in (year, month, day)

invalidYearMonthDays :: [(Year, Month, Day)]
invalidYearMonthDays =
  Set.toList (Set.fromList allYearMonthDays \\ Set.fromList validYearMonthDays)

allYearMonthDays :: [(Year, Month, Day)]
allYearMonthDays = liftA3 (,,) years months days

test_YearMonthDay_bounds :: TestTree
test_YearMonthDay_bounds = testBounds (ymd 0000 01 01, ymd 9999 12 31)

--
-- YearMonth
--
test_YearMonth :: TestTree
test_YearMonth =
  testGroup
    "YearMonth"
    [ test_YearMonth_bounds
    ]

test_YearMonth_bounds :: TestTree
test_YearMonth_bounds = testBounds (ym 0000 01, ym 9999 12)

--
-- MonthDay
--
test_MonthDay :: TestTree
test_MonthDay =
  testGroup
    "MonthYear"
    [ test_MonthDay_mkMonthDay,
      test_MonthDay_bounds
    ]

test_MonthDay_mkMonthDay :: TestTree
test_MonthDay_mkMonthDay =
  testGroup
    "mkMonthDay"
    [ test_MonthDay_mkMonthDay_unit,
      test_MonthDay_mkMonthDay_exhaustive
    ]

test_MonthDay_mkMonthDay_unit :: TestTree
test_MonthDay_mkMonthDay_unit =
  testGroup
    "unit"
    [ test_MonthDay_mkMonthDay_unit_valid,
      test_MonthDay_mkMonthDay_unit_invalid
    ]

test_MonthDay_mkMonthDay_exhaustive :: TestTree
test_MonthDay_mkMonthDay_exhaustive =
  testGroup
    "exhaustive"
    [ test_MonthDay_mkMonthDay_exhaustive_valid,
      test_MonthDay_mkMonthDay_exhaustive_invalid
    ]

test_MonthDay_mkMonthDay_unit_valid :: TestTree
test_MonthDay_mkMonthDay_unit_valid = unimplemented "valid"

test_MonthDay_mkMonthDay_unit_invalid :: TestTree
test_MonthDay_mkMonthDay_unit_invalid = unimplemented "invalid"

test_MonthDay_mkMonthDay_exhaustive_valid :: TestTree
test_MonthDay_mkMonthDay_exhaustive_valid =
  testCase "valid" $
    forM_ validMonthDays $ \(month, day) -> do
      monthDay <-
        case mkMonthDay month day of
          Nothing ->
            assertFailure $
              "could not create MonthDay from "
                <> show month
                <> " and "
                <> show day
          Just x -> pure x
      getMonth monthDay @?= month
      getDay monthDay @?= day

test_MonthDay_mkMonthDay_exhaustive_invalid :: TestTree
test_MonthDay_mkMonthDay_exhaustive_invalid =
  testCase "invalid" $
    forM_ invalidMonthDays $ \(month, day) ->
      assertBool "made invalid MonthDay" (isNothing (mkMonthDay month day))

validMonthDays :: [(Month, Day)]
validMonthDays = filter (uncurry isValid) allMonthDays
  where
    isValid :: Month -> Day -> Bool
    isValid month day =
      let monthOfYear :: MonthOfYear
          monthOfYear = (+ 1) . fromInteger . getFinite . unMonth $ month

          dayOfMonth :: DayOfMonth
          dayOfMonth = (+ 1) . fromInteger . getFinite . unDay $ day

          isLeapYear :: Bool
          isLeapYear = True
       in isJust (monthAndDayToDayOfYearValid isLeapYear monthOfYear dayOfMonth)

invalidMonthDays :: [(Month, Day)]
invalidMonthDays =
  Set.toList (Set.fromList allMonthDays \\ Set.fromList validMonthDays)

allMonthDays :: [(Month, Day)]
allMonthDays = liftA2 (,) months days

test_MonthDay_bounds :: TestTree
test_MonthDay_bounds = testBounds (md 01 01, md 12 31)

-- =========
-- UTILITIES
-- =========

--------------------------------
-- Year, Month, Day enumerations
--------------------------------

-- All `Year`s, `Month`s, and `Day`s
years :: [Year]
years = map Year finites

months :: [Month]
months = map Month finites

days :: [Day]
days = map Day finites

-- All `Text`s that represent semantically valid `Year`s, `Month`s, or `Day`s,
-- paired with their parsed value.
exhaustive_Year_valid :: [(Text, Year)]
exhaustive_Year_valid =
  let texts = map Text.pack $ replicateM 4 ['0' .. '9']
   in zip texts years

exhaustive_Month_valid :: [(Text, Month)]
exhaustive_Month_valid =
  let texts = map (Text.justifyRight 2 '0' . showt @Int) [1 .. 12]
   in zip texts months

exhaustive_Day_valid :: [(Text, Day)]
exhaustive_Day_valid =
  let texts = map (Text.justifyRight 2 '0' . showt @Int) [1 .. 31]
   in zip texts days

--------------------
-- Testing functions
--------------------
testParseValid :: (Show a, Eq a, HasParser a) => [(Text, a)] -> TestTree
testParseValid cases =
  testCase "valid" $
    forM_ cases $ \(text, value) ->
      parse text @?= Just value

testParseInvalid ::
  forall a. (Show a, Eq a, HasParser a) => Proxy a -> [Text] -> TestTree
testParseInvalid _ cases =
  testCase "invalid" $
    forM_ cases $ \text ->
      parse text @?= (Nothing :: Maybe a)

testSerialize :: (HasSerializer a) => TestName -> [(Text, a)] -> TestTree
testSerialize name cases =
  testCase name $
    forM_ cases $ \(text, value) ->
      serialize value @?= text

testBounds :: forall a. (Show a, Eq a, Bounded a) => (a, a) -> TestTree
testBounds (expectedMinBound, expectedMaxBound) =
  testCase "bounds" $ do
    minBound @?= expectedMinBound
    maxBound @?= expectedMaxBound

--------------------------------
-- Year, Month, Day construction
--------------------------------
y :: Integer -> Year
y year = Year (finite year)

m :: Integer -> Month
m month = Month (finite (month - 1))

d :: Integer -> Day
d day = Day (finite (day - 1))

ymd :: Integer -> Integer -> Integer -> YearMonthDay
ymd year month day = YearMonthDay (y year) (m month) (d day)

ym :: Integer -> Integer -> YearMonth
ym year month = YearMonth (y year) (m month)

md :: Integer -> Integer -> MonthDay
md month day = MonthDay (m month) (d day)

unimplemented :: TestName -> TestTree
unimplemented name =
  ignoreTestBecause "UNIMPLEMENTED" $
    testCase name (assertFailure "")
