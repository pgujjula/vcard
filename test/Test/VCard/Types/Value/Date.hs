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
import Data.List.Ordered (minus)
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
import Test.Tasty (TestName, TestTree, testGroup)
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
      test_Year_parse_unit_invalidSemantics,
      test_Year_parse_unit_invalidSyntax
    ]

test_Year_parse_exhaustive :: TestTree
test_Year_parse_exhaustive =
  testGroup
    "exhaustive"
    [ test_Year_parse_exhaustive_valid,
      test_Year_parse_exhaustive_invalidSemantics
    ]

test_Year_parse_unit_valid :: TestTree
test_Year_parse_unit_valid =
  testParseValid units_Year_valid

test_Year_parse_unit_invalidSemantics :: TestTree
test_Year_parse_unit_invalidSemantics =
  testParseInvalidSemantics (Proxy @Year) units_Year_invalidSemantics

test_Year_parse_unit_invalidSyntax :: TestTree
test_Year_parse_unit_invalidSyntax =
  testParseInvalidSyntax (Proxy @Year) units_Year_invalidSyntax

test_Year_parse_exhaustive_valid :: TestTree
test_Year_parse_exhaustive_valid =
  testParseValid exhaustive_Year_valid

test_Year_parse_exhaustive_invalidSemantics :: TestTree
test_Year_parse_exhaustive_invalidSemantics =
  testParseInvalidSemantics (Proxy @Year) exhaustive_Year_invalid

test_Year_serialize :: TestTree
test_Year_serialize =
  testGroup
    "serialize"
    [ test_Year_serialize_unit,
      test_Year_serialize_exhaustive
    ]

test_Year_serialize_unit :: TestTree
test_Year_serialize_unit = testSerialize "unit" units_Year_valid

test_Year_serialize_exhaustive :: TestTree
test_Year_serialize_exhaustive =
  testSerialize "exhaustive" exhaustive_Year_valid

test_Year_bounds :: TestTree
test_Year_bounds = testBounds (y 0000, y 9999)

units_Year_valid :: [(Text, Year)]
units_Year_valid =
  [ ("0000", Year 0000),
    ("6812", Year 6812),
    ("9999", Year 9999)
  ]

-- All syntactically valid Years are semantically valid
units_Year_invalidSemantics :: [Text]
units_Year_invalidSemantics = []

units_Year_invalidSyntax :: [Text]
units_Year_invalidSyntax =
  concat
    [ -- incorrect number of digits
      ["1", "01", "001", "00001"],
      ["25", "025", "00025"],
      -- negative numbers
      ["-1", "-01", "-001", "-0001", "-00001"],
      ["-25", "-025", "-0025", "-00025"],
      -- too large numbers
      ["10000", "010000"],
      ["92893", "092893"],
      -- invalid number formats
      ["2e3", "1000.0"],
      -- invalid characters
      ["15a1", "1b51"],
      -- leading or trailing whitespace
      [" 3275", "\n3275", "\r\n3275", "3275 ", "3275\n", "3275\r\n"]
    ]

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
      test_Month_parse_unit_invalidSemantics,
      test_Month_parse_unit_invalidSyntax
    ]

test_Month_parse_exhaustive :: TestTree
test_Month_parse_exhaustive =
  testGroup
    "exhaustive"
    [ test_Month_parse_exhaustive_valid,
      test_Month_parse_exhaustive_invalid
    ]

test_Month_parse_unit_valid :: TestTree
test_Month_parse_unit_valid = testParseValid units_Month_valid

test_Month_parse_unit_invalidSemantics :: TestTree
test_Month_parse_unit_invalidSemantics =
  testParseInvalidSemantics (Proxy @Month) units_Month_invalidSemantics

test_Month_parse_unit_invalidSyntax :: TestTree
test_Month_parse_unit_invalidSyntax =
  testParseInvalidSyntax (Proxy @Month) units_Month_invalidSyntax

test_Month_parse_exhaustive_valid :: TestTree
test_Month_parse_exhaustive_valid = testParseValid exhaustive_Month_valid

test_Month_parse_exhaustive_invalid :: TestTree
test_Month_parse_exhaustive_invalid =
  testParseInvalidSemantics (Proxy @Month) exhaustive_Month_invalid

test_Month_serialize :: TestTree
test_Month_serialize =
  testGroup
    "serialize"
    [ test_Month_serialize_unit,
      test_Month_serialize_exhaustive
    ]

test_Month_serialize_unit :: TestTree
test_Month_serialize_unit = testSerialize "unit" units_Month_valid

test_Month_serialize_exhaustive :: TestTree
test_Month_serialize_exhaustive =
  testSerialize "exhaustive" exhaustive_Month_valid

test_Month_bounds :: TestTree
test_Month_bounds = testBounds (m 01, m 12)

units_Month_valid :: [(Text, Month)]
units_Month_valid =
  [("01", m 01), ("05", m 05), ("10", m 10), ("12", m 12)]

units_Month_invalidSemantics :: [Text]
units_Month_invalidSemantics =
  ["00", "13", "14", "15", "20", "99"]

units_Month_invalidSyntax :: [Text]
units_Month_invalidSyntax =
  concat
    [ -- incorrect number of digits
      ["1", "001", "0001"],
      ["7", "007", "0007"],
      ["010", "0010"],
      -- negative numbers
      ["-1", "-01", "-001", "-0001"],
      ["-11", "-011", "-0011", "-00011"],
      -- too small/large numbers
      ["0", "000", "0000"],
      ["013", "0013"],
      ["020", "0020"],
      -- invalid number formats
      ["1e1", "10.0"],
      -- invalid characters
      ["a", "1a", "a1"],
      -- leading or trailing whitespace
      [" 07", "\n07", "\r\n07", "07 ", "07\n", "07\r\n"]
    ]

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
      test_Day_parse_unit_invalidSemantics,
      test_Day_parse_unit_invalidSyntax
    ]

test_Day_parse_exhaustive :: TestTree
test_Day_parse_exhaustive =
  testGroup
    "exhaustive"
    [ test_Day_parse_exhaustive_valid,
      test_Day_parse_exhaustive_invalid
    ]

test_Day_parse_unit_valid :: TestTree
test_Day_parse_unit_valid = testParseValid units_Day_valid

test_Day_parse_unit_invalidSemantics :: TestTree
test_Day_parse_unit_invalidSemantics =
  testParseInvalidSemantics (Proxy @Day) units_Day_invalidSemantics

test_Day_parse_unit_invalidSyntax :: TestTree
test_Day_parse_unit_invalidSyntax =
  testParseInvalidSemantics (Proxy @Day) units_Day_invalidSyntax

test_Day_parse_exhaustive_valid :: TestTree
test_Day_parse_exhaustive_valid = testParseValid exhaustive_Day_valid

test_Day_parse_exhaustive_invalid :: TestTree
test_Day_parse_exhaustive_invalid =
  testParseInvalidSemantics (Proxy @Day) exhaustive_Day_invalid

test_Day_serialize :: TestTree
test_Day_serialize =
  testGroup
    "serialize"
    [ test_Day_serialize_unit,
      test_Day_serialize_exhaustive
    ]

test_Day_serialize_unit :: TestTree
test_Day_serialize_unit = testSerialize "unit" units_Day_valid

test_Day_serialize_exhaustive :: TestTree
test_Day_serialize_exhaustive = testSerialize "exhaustive" exhaustive_Day_valid

test_Day_bounds :: TestTree
test_Day_bounds = testBounds (d 01, d 31)

units_Day_valid :: [(Text, Day)]
units_Day_valid =
  [("01", d 1), ("15", d 15), ("30", d 30), ("31", d 31)]

units_Day_invalidSemantics :: [Text]
units_Day_invalidSemantics =
  ["00", "32", "33", "34", "50", "99"]

units_Day_invalidSyntax :: [Text]
units_Day_invalidSyntax =
  concat
    [ -- incorrect number of digits
      ["1", "001", "0001"],
      ["7", "007", "0007"],
      ["030", "0030"],
      ["031", "0031"],
      -- negative numbers
      ["-1", "-01", "-001", "-0001"],
      ["-12", "-012", "-0012"],
      -- too small/large numbers
      ["0", "000", "0000"],
      ["032", "0032"],
      ["099", "0099"],
      -- invalid number formats
      ["1e1", "10.0"],
      -- invalid characters
      ["a", "1a", "a1"],
      -- leading or trailing whitespace
      [" 07", "\n07", "\r\n07", "07 ", "07\n", "07\r\n"]
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

test_YearMonthDay_mkYearMonthDay_unit_valid :: TestTree
test_YearMonthDay_mkYearMonthDay_unit_valid =
  testMkYearMonthDayValid units_YearMonthDay_valid

test_YearMonthDay_mkYearMonthDay_unit_invalid :: TestTree
test_YearMonthDay_mkYearMonthDay_unit_invalid =
  testMkYearMonthDayInvalid units_YearMonthDay_invalid

test_YearMonthDay_mkYearMonthDay_exhaustive :: TestTree
test_YearMonthDay_mkYearMonthDay_exhaustive =
  testCase "exhaustive" $
    forM_ (liftA3 (,,) years months days) $
      \(year, month, day) ->
        mkYearMonthDay year month day @?= timeMkYearMonthDay year month day

test_YearMonthDay_bounds :: TestTree
test_YearMonthDay_bounds = testBounds (ymd 0000 01 01, ymd 9999 12 31)

units_YearMonthDay_valid :: [(Year, Month, Day)]
units_YearMonthDay_valid =
  [ -- bounds
    (y 0000, m 01, d 01),
    (y 9999, m 12, d 31),
    -- vary year
    (y 0000, m 04, d 12),
    (y 5317, m 04, d 12),
    (y 9999, m 04, d 12),
    -- vary month
    (y 4771, m 01, d 12),
    (y 4771, m 07, d 12),
    (y 4771, m 12, d 12),
    -- vary day
    (y 3909, m 08, d 01),
    (y 3909, m 08, d 17),
    (y 3909, m 08, d 31),
    -- max day
    (y 5317, m 01, d 31),
    (y 5317, m 02, d 28),
    (y 5317, m 03, d 31),
    (y 5317, m 04, d 30),
    (y 5317, m 05, d 31),
    (y 5317, m 06, d 30),
    (y 5317, m 07, d 31),
    (y 5317, m 08, d 31),
    (y 5317, m 09, d 30),
    (y 5317, m 10, d 31),
    (y 5317, m 11, d 30),
    (y 5317, m 12, d 31),
    -- non leap years (not multiples of 4)
    (y 0001, m 02, d 28),
    (y 4922, m 02, d 28),
    (y 9999, m 02, d 28),
    -- leap years (multiples of 4 but not of 100)
    (y 0004, m 02, d 28),
    (y 0004, m 02, d 29),
    (y 6784, m 02, d 28),
    (y 6784, m 02, d 29),
    (y 9996, m 02, d 28),
    (y 9996, m 02, d 29),
    -- non leap years (multiples of 100 but not of 400)
    (y 0100, m 02, d 28),
    (y 6700, m 02, d 28),
    (y 9900, m 02, d 28),
    -- leap years (multiples of 400)
    (y 0000, m 02, d 28),
    (y 0000, m 02, d 29),
    (y 6800, m 02, d 28),
    (y 6800, m 02, d 29),
    (y 9600, m 02, d 28),
    (y 9600, m 02, d 29)
  ]

units_YearMonthDay_invalid :: [(Year, Month, Day)]
units_YearMonthDay_invalid =
  [ -- beyond max day
    (y 5317, m 02, d 29),
    (y 5317, m 02, d 30),
    (y 5317, m 02, d 31),
    (y 5317, m 04, d 31),
    (y 5317, m 06, d 31),
    (y 5317, m 09, d 31),
    (y 5317, m 11, d 31),
    -- non leap years (not multiples of 4)
    (y 0001, m 02, d 29),
    (y 0001, m 02, d 30),
    (y 0001, m 02, d 31),
    (y 4922, m 02, d 29),
    (y 4922, m 02, d 30),
    (y 4922, m 02, d 31),
    (y 9999, m 02, d 29),
    (y 9999, m 02, d 30),
    (y 9999, m 02, d 31),
    -- leap years (multiples of 4 but not of 100)
    (y 0004, m 02, d 30),
    (y 0004, m 02, d 31),
    (y 6784, m 02, d 30),
    (y 6784, m 02, d 31),
    (y 9996, m 02, d 30),
    (y 9996, m 02, d 31),
    -- non leap years (multiples of 100 but not of 400)
    (y 0100, m 02, d 29),
    (y 0100, m 02, d 30),
    (y 0100, m 02, d 31),
    (y 6700, m 02, d 29),
    (y 6700, m 02, d 30),
    (y 6700, m 02, d 31),
    (y 9900, m 02, d 29),
    (y 9900, m 02, d 30),
    (y 9900, m 02, d 31),
    -- leap years (multiples of 400)
    (y 0000, m 02, d 30),
    (y 0000, m 02, d 31),
    (y 6800, m 02, d 30),
    (y 6800, m 02, d 31),
    (y 9600, m 02, d 30),
    (y 9600, m 02, d 31)
  ]

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
test_MonthDay_mkMonthDay_unit_valid =
  testMkMonthDayValid units_MonthDay_valid

test_MonthDay_mkMonthDay_unit_invalid :: TestTree
test_MonthDay_mkMonthDay_unit_invalid =
  testMkMonthDayInvalid units_MonthDay_invalid

test_MonthDay_mkMonthDay_exhaustive_valid :: TestTree
test_MonthDay_mkMonthDay_exhaustive_valid =
  testMkMonthDayValid validMonthDays

test_MonthDay_mkMonthDay_exhaustive_invalid :: TestTree
test_MonthDay_mkMonthDay_exhaustive_invalid =
  testMkMonthDayInvalid invalidMonthDays

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

units_MonthDay_valid :: [(Month, Day)]
units_MonthDay_valid =
  [ -- bounds
    (m 01, d 01),
    (m 12, d 31),
    -- vary month
    (m 01, d 12),
    (m 07, d 12),
    (m 12, d 12),
    -- vary day
    (m 07, d 01),
    (m 07, d 12),
    (m 07, d 31),
    -- max day
    (m 01, d 31),
    (m 02, d 28),
    (m 02, d 29), -- February includes leap day
    (m 03, d 31),
    (m 04, d 30),
    (m 05, d 31),
    (m 06, d 30),
    (m 07, d 31),
    (m 08, d 31),
    (m 09, d 30),
    (m 10, d 31),
    (m 11, d 30),
    (m 12, d 31)
  ]

units_MonthDay_invalid :: [(Month, Day)]
units_MonthDay_invalid =
  [ -- beyond max day
    (m 02, d 30),
    (m 02, d 31),
    (m 04, d 31),
    (m 06, d 31),
    (m 09, d 31),
    (m 11, d 31)
  ]

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

-- All `Text`s in the syntactic format of a `Year` (yyyy), `Month` (mm), or
-- `Day` (mm), but not necessarily semantically valid.
universe_Year :: [Text]
universe_Year = map Text.pack (replicateM 4 ['0' .. '9'])

universe_Month :: [Text]
universe_Month = map Text.pack (replicateM 2 ['0' .. '9'])

universe_Day :: [Text]
universe_Day = map Text.pack (replicateM 2 ['0' .. '9'])

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

-- `Text`s that fit the syntactic format of `Year`, `Month`, `Date`, but are not
-- semantically valid. Example: "13" is in exhaustive_Month_invalid
exhaustive_Year_invalid :: [Text]
exhaustive_Year_invalid = universe_Year `minus` map fst exhaustive_Year_valid

exhaustive_Month_invalid :: [Text]
exhaustive_Month_invalid = universe_Month `minus` map fst exhaustive_Month_valid

exhaustive_Day_invalid :: [Text]
exhaustive_Day_invalid = universe_Day `minus` map fst exhaustive_Day_valid

--------------------
-- Testing functions
--------------------

testParseValid :: (Show a, Eq a, HasParser a) => [(Text, a)] -> TestTree
testParseValid cases =
  testCase "valid" $
    forM_ cases $ \(text, value) ->
      parse text @?= Just value

testParseInvalid ::
  forall a.
  (Show a, Eq a, HasParser a) =>
  TestName ->
  Proxy a ->
  [Text] ->
  TestTree
testParseInvalid name _ cases =
  testCase name $
    forM_ cases $ \text ->
      parse text @?= (Nothing :: Maybe a)

testParseInvalidSemantics ::
  (Show a, Eq a, HasParser a) => Proxy a -> [Text] -> TestTree
testParseInvalidSemantics = testParseInvalid "invalid_semantics"

testParseInvalidSyntax ::
  (Show a, Eq a, HasParser a) => Proxy a -> [Text] -> TestTree
testParseInvalidSyntax = testParseInvalid "invalid_syntax"

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

testMkYearMonthDayValid :: [(Year, Month, Day)] -> TestTree
testMkYearMonthDayValid cases =
  testCase "valid" $
    forM_ cases $ \(year, month, day) -> do
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

testMkYearMonthDayInvalid :: [(Year, Month, Day)] -> TestTree
testMkYearMonthDayInvalid cases =
  testCase "invalid" $
    forM_ cases $ \(year, month, day) ->
      case mkYearMonthDay year month day of
        Nothing -> pure ()
        Just yearMonthDay ->
          assertFailure $
            "made invalid "
              <> show yearMonthDay

testMkMonthDayValid :: [(Month, Day)] -> TestTree
testMkMonthDayValid cases =
  testCase "valid" $
    forM_ cases $ \(month, day) -> do
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

testMkMonthDayInvalid :: [(Month, Day)] -> TestTree
testMkMonthDayInvalid cases =
  testCase "invalid" $
    forM_ cases $ \(month, day) ->
      assertBool "made invalid MonthDay" (isNothing (mkMonthDay month day))

------------------------------------
-- Functions with the `time` library
------------------------------------
timeMkYearMonthDay :: Year -> Month -> Day -> Maybe YearMonthDay
timeMkYearMonthDay year month day =
  let isValid =
        isJust $
          Time.fromGregorianValid
            (toTimeYear year)
            (toTimeMonth month)
            (toTimeDay day)
   in if isValid
        then Just (YearMonthDay year month day)
        else Nothing

toTimeYear :: Year -> Time.Year
toTimeYear = getFinite . unYear

toTimeMonth :: Month -> Time.MonthOfYear
toTimeMonth = fromInteger . (+ 1) . getFinite . unMonth

toTimeDay :: Day -> Time.DayOfMonth
toTimeDay = fromInteger . (+ 1) . getFinite . unDay

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
