-- SPDX-FileCopyrightText: Copyright Preetham Gujjula
-- SPDX-License-Identifier: BSD-3-Clause
{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
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
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, assertFailure, testCase, (@?=))
import TextShow (showt)
import VCard.Parse (parse)
import VCard.Serialize (serialize)
import VCard.Types.Value.Date
  ( Day (..),
    Month (..),
    Year (..),
    YearMonth (..),
    getDay,
    getMonth,
    getYear,
    mkMonthDay,
    mkYearMonthDay,
  )

tests :: TestTree
tests =
  testGroup
    "Date"
    [ dayTests,
      monthTests,
      yearTests,
      yearMonthDayTests,
      yearMonthTests,
      monthDayTests
    ]

--
-- Day
--
dayTests :: TestTree
dayTests = testGroup "Day" [validDayTests, invalidDayTests, dayBoundsTests]

validDayTests :: TestTree
validDayTests = testCase "valid" $ do
  forM_ validDays $ \(text, value) -> do
    parse text @?= Just value
    serialize value @?= text

validDays :: [(Text, Day)]
validDays = singleDigits ++ doubleDigits
  where
    singleDigits :: [(Text, Day)]
    singleDigits = flip map [1 .. 9] $ \i ->
      ("0" <> showt i, Day (finite (i - 1)))

    doubleDigits :: [(Text, Day)]
    doubleDigits = flip map [10 .. 31] $ \i ->
      (showt i, Day (finite (i - 1)))

invalidDayTests :: TestTree
invalidDayTests =
  testCase "invalid" $
    forM_ invalidDays $ \text -> do
      parse @Day text @?= Nothing

dayBoundsTests :: TestTree
dayBoundsTests = testCase "bounds" $ do
  minBound @?= Day (finite 0)
  maxBound @?= Day (finite 30)

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
-- Month
--
monthTests :: TestTree
monthTests =
  testGroup
    "Month"
    [ validMonthTests,
      invalidMonthTests,
      monthBoundsTests
    ]

validMonthTests :: TestTree
validMonthTests = testCase "valid" $ do
  forM_ validMonths $ \(text, value) -> do
    parse text @?= Just value
    serialize value @?= text

validMonths :: [(Text, Month)]
validMonths =
  [ ("01", Month (finite 0)),
    ("02", Month (finite 1)),
    ("03", Month (finite 2)),
    ("04", Month (finite 3)),
    ("05", Month (finite 4)),
    ("06", Month (finite 5)),
    ("07", Month (finite 6)),
    ("08", Month (finite 7)),
    ("09", Month (finite 8)),
    ("10", Month (finite 9)),
    ("11", Month (finite 10)),
    ("12", Month (finite 11))
  ]

invalidMonthTests :: TestTree
invalidMonthTests =
  testCase "invalid" $
    forM_ invalidMonths $ \text -> do
      parse @Month text @?= Nothing

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

monthBoundsTests :: TestTree
monthBoundsTests = testCase "bounds" $ do
  minBound @?= Month (finite 0)
  maxBound @?= Month (finite 11)

--
-- Year
--
yearTests :: TestTree
yearTests = testGroup "Year" [validYearTests, invalidYearTests, yearBoundsTests]

validYearTests :: TestTree
validYearTests = testCase "valid" $ do
  forM_ validYears $ \(text, value) -> do
    parse text @?= Just value
    serialize value @?= text

validYears :: [(Text, Year)]
validYears = zip yearTexts years
  where
    yearTexts :: [Text]
    yearTexts = map Text.pack $ replicateM 4 ['0' .. '9']

    years :: [Year]
    years = map (Year . finite) [0 .. 9999]

invalidYearTests :: TestTree
invalidYearTests =
  testCase "invalid" $
    forM_ invalidYears $ \text -> do
      parse @Year text @?= Nothing

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

yearBoundsTests :: TestTree
yearBoundsTests = testCase "bounds" $ do
  minBound @?= Year (finite 0)
  maxBound @?= Year (finite 9999)

--
-- YearMonthDay
--
yearMonthDayTests :: TestTree
yearMonthDayTests =
  testGroup
    "YearMonthDay"
    [ mkYearMonthDayTests,
      yearMonthDayBoundsTests
    ]

mkYearMonthDayTests :: TestTree
mkYearMonthDayTests =
  testGroup
    "mkYearMonthDay"
    [ validMkYearMonthDayTests,
      invalidMkYearMonthDayTests
    ]

validMkYearMonthDayTests :: TestTree
validMkYearMonthDayTests =
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

invalidMkYearMonthDayTests :: TestTree
invalidMkYearMonthDayTests = testCase "invalid" $
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
      year = Year (finite timeYear)

      month :: Month
      month = Month (finite (toInteger (timeMonthOfYear - 1)))

      day :: Day
      day = Day (finite (toInteger (timeDayOfMonth - 1)))
   in (year, month, day)

invalidYearMonthDays :: [(Year, Month, Day)]
invalidYearMonthDays =
  Set.toList (Set.fromList allYearMonthDays \\ Set.fromList validYearMonthDays)

allYearMonthDays :: [(Year, Month, Day)]
allYearMonthDays =
  let years = map Year finites
      months = map Month finites
      days = map Day finites
   in liftA3 (,,) years months days

yearMonthDayBoundsTests :: TestTree
yearMonthDayBoundsTests = testCase "bounds" $ do
  Just minBound
    @?= mkYearMonthDay (Year (finite 0)) (Month (finite 0)) (Day (finite 0))
  Just maxBound
    @?= mkYearMonthDay
      (Year (finite 9999))
      (Month (finite 11))
      (Day (finite 30))

--
-- YearMonth
--
yearMonthTests :: TestTree
yearMonthTests = testGroup "YearMonth" [yearMonthBoundsTests]

yearMonthBoundsTests :: TestTree
yearMonthBoundsTests = testCase "bounds" $ do
  minBound @?= YearMonth (Year (finite 0)) (Month (finite 0))
  maxBound @?= YearMonth (Year (finite 9999)) (Month (finite 11))

--
-- MonthDay
--
monthDayTests :: TestTree
monthDayTests = testGroup "MonthYear" [mkMonthDayTests, monthDayBoundsTests]

mkMonthDayTests :: TestTree
mkMonthDayTests =
  testGroup
    "mkMonthDay"
    [ validMkMonthDayTests,
      invalidMkMonthDayTests
    ]

validMkMonthDayTests :: TestTree
validMkMonthDayTests = testCase "valid" $
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

invalidMkMonthDayTests :: TestTree
invalidMkMonthDayTests = testCase "invalid" $ do
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
allMonthDays =
  let months = map Month finites
      days = map Day finites
   in liftA2 (,) months days

monthDayBoundsTests :: TestTree
monthDayBoundsTests = testCase "bounds" $ do
  Just minBound @?= mkMonthDay (Month (finite 0)) (Day (finite 0))
  Just maxBound @?= mkMonthDay (Month (finite 11)) (Day (finite 30))
