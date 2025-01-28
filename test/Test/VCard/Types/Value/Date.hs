-- SPDX-FileCopyrightText: Copyright Preetham Gujjula
-- SPDX-License-Identifier: BSD-3-Clause
{-# LANGUAGE CPP #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module Test.VCard.Types.Value.Date (tests) where

#if !MIN_VERSION_base(4,18,0)
import Control.Applicative (liftA2, liftA3)
#else
import Control.Applicative (liftA3)
#endif
import Control.Monad (forM_, replicateM)
import Data.Bifunctor (second)
import Data.Finite (finite, finites, getFinite)
import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.List.NonEmpty qualified as NonEmpty
import Data.List.Ordered (minus)
import Data.Maybe (isJust, isNothing, maybeToList)
import Data.Proxy (Proxy (..))
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Time qualified as Time
import Data.Time.Calendar.MonthDay
  ( monthAndDayToDayOfYearValid,
  )
import Test.Tasty (TestName, TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, assertFailure, testCase, (@?=))
import TextShow (showt)
import VCard.Parse (HasParser, parse)
import VCard.Serialize (HasSerializer, serialize)
import VCard.Types.Value.Date
  ( Date (..),
    DateList,
    Day (..),
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
import VCard.Types.Value.List (List (..))
import Vary ((:|))
import Vary qualified

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
      test_MonthDay,
      test_Date,
      test_DateList
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
    [ testGroup
        "unit"
        [ testParseValid units_Year_valid,
          testParseInvalidSemantics (Proxy @Year) units_Year_invalidSemantics,
          testParseInvalidSyntax (Proxy @Year) units_Year_invalidSyntax
        ],
      testGroup
        "exhaustive"
        [ testParseValid exhaustive_Year_valid,
          testParseInvalidSemantics (Proxy @Year) exhaustive_Year_invalid
        ]
    ]

test_Year_serialize :: TestTree
test_Year_serialize =
  testGroup
    "serialize"
    [ testSerialize "unit" units_Year_valid,
      testSerialize "exhaustive" exhaustive_Year_valid
    ]

test_Year_bounds :: TestTree
test_Year_bounds = testBounds (y 0000, y 9999)

-- See also: units_DateLike_valid_Year
units_Year_valid :: [(Text, Year)]
units_Year_valid =
  [ ("0000", Year 0000),
    ("6812", Year 6812),
    ("9999", Year 9999)
  ]

-- All syntactically valid Years are semantically valid
units_Year_invalidSemantics :: [Text]
units_Year_invalidSemantics = []

-- See also: units_Date_invalidSyntax_Year
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
    [ testGroup
        "unit"
        [ testParseValid units_Month_valid,
          testParseInvalidSemantics (Proxy @Month) units_Month_invalidSemantics,
          testParseInvalidSyntax (Proxy @Month) units_Month_invalidSyntax
        ],
      testGroup
        "exhaustive"
        [ testParseValid exhaustive_Month_valid,
          testParseInvalidSemantics (Proxy @Month) exhaustive_Month_invalid
        ]
    ]

test_Month_serialize :: TestTree
test_Month_serialize =
  testGroup
    "serialize"
    [ testSerialize "unit" units_Month_valid,
      testSerialize "exhaustive" exhaustive_Month_valid
    ]

test_Month_bounds :: TestTree
test_Month_bounds = testBounds (m 01, m 12)

-- See also: units_DateLike_valid_Month
units_Month_valid :: [(Text, Month)]
units_Month_valid =
  [("01", m 01), ("05", m 05), ("10", m 10), ("12", m 12)]

-- See also: units_DateLike_invalidSemantics_Month
units_Month_invalidSemantics :: [Text]
units_Month_invalidSemantics =
  ["00", "13", "14", "15", "20", "99"]

-- See also: units_Date_invalidSyntax_Month
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
    [ testGroup
        "unit"
        [ testParseValid units_Day_valid,
          testParseInvalidSemantics (Proxy @Day) units_Day_invalidSemantics,
          testParseInvalidSyntax (Proxy @Day) units_Day_invalidSyntax
        ],
      testGroup
        "exhaustive"
        [ testParseValid exhaustive_Day_valid,
          testParseInvalidSemantics (Proxy @Day) exhaustive_Day_invalid
        ]
    ]

test_Day_serialize :: TestTree
test_Day_serialize =
  testGroup
    "serialize"
    [ testSerialize "unit" units_Day_valid,
      testSerialize "exhaustive" exhaustive_Day_valid
    ]

test_Day_bounds :: TestTree
test_Day_bounds = testBounds (d 01, d 31)

-- See also: units_DateLike_valid_Day
units_Day_valid :: [(Text, Day)]
units_Day_valid =
  [("01", d 1), ("15", d 15), ("30", d 30), ("31", d 31)]

-- See also: units_DateLike_invalidSemantics_Day
units_Day_invalidSemantics :: [Text]
units_Day_invalidSemantics =
  ["00", "32", "33", "34", "50", "99"]

-- See also: units_Date_invalidSyntax_Day
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
  testCase "valid" $
    forM_ units_YearMonthDay_valid $ \(year, month, day) -> do
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

test_YearMonthDay_mkYearMonthDay_unit_invalid :: TestTree
test_YearMonthDay_mkYearMonthDay_unit_invalid =
  testCase "invalid" $
    forM_ units_YearMonthDay_invalid $ \(year, month, day) ->
      assertBool
        "made invalid YearMonthDay"
        (isNothing (mkYearMonthDay year month day))

test_YearMonthDay_mkYearMonthDay_exhaustive :: TestTree
test_YearMonthDay_mkYearMonthDay_exhaustive =
  testCase "exhaustive" $
    forM_ (liftA3 (,,) years months days) $
      \(year, month, day) ->
        mkYearMonthDay year month day @?= timeMkYearMonthDay year month day

test_YearMonthDay_bounds :: TestTree
test_YearMonthDay_bounds = testBounds (ymd 0000 01 01, ymd 9999 12 31)

-- See also: units_DateLike_valid_YearMonthDay
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

-- See also: units_DateLike_invalidSemantics_YearMonthDay
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

test_MonthDay_mkMonthDay_unit_valid :: TestTree
test_MonthDay_mkMonthDay_unit_valid =
  testCase "valid" $
    forM_ units_MonthDay_valid $ \(month, day) -> do
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

test_MonthDay_mkMonthDay_unit_invalid :: TestTree
test_MonthDay_mkMonthDay_unit_invalid =
  testCase "invalid" $
    forM_ units_MonthDay_invalid $ \(month, day) ->
      assertBool "made invalid MonthDay" (isNothing (mkMonthDay month day))

test_MonthDay_mkMonthDay_exhaustive :: TestTree
test_MonthDay_mkMonthDay_exhaustive =
  testCase "exhaustive" $
    forM_ (liftA2 (,) months days) $ \(month, day) ->
      mkMonthDay month day @?= timeMkMonthDay month day

test_MonthDay_bounds :: TestTree
test_MonthDay_bounds = testBounds (md 01 01, md 12 31)

-- See also: units_DateLike_valid_MonthDay
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

-- See also: units_DateLike_invalidSemantics_MonthDay
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

--
-- Date
--
test_Date :: TestTree
test_Date =
  testGroup
    "Date"
    [ test_Date_parse,
      test_Date_serialize
    ]

test_Date_parse :: TestTree
test_Date_parse =
  testGroup
    "parse"
    [ testGroup
        "unit"
        [ testParseValid units_Date_valid,
          testParseInvalidSemantics (Proxy @Date) units_Date_invalidSemantics,
          testParseInvalidSyntax (Proxy @Date) units_Date_invalidSyntax
        ],
      testGroup
        "exhaustive"
        [ testParseValid exhaustive_Date_valid,
          testParseInvalidSemantics (Proxy @Date) exhaustive_Date_invalid
        ]
    ]

test_Date_serialize :: TestTree
test_Date_serialize =
  testGroup
    "serialize"
    [ testSerialize "unit" units_Date_valid,
      testSerialize "exhaustive" exhaustive_Date_valid
    ]

units_Date_valid :: [(Text, Date)]
units_Date_valid =
  concat
    [ pack units_DateLike_valid_Year,
      pack units_DateLike_valid_Month,
      pack units_DateLike_valid_Day,
      pack units_DateLike_valid_YearMonthDay,
      pack units_DateLike_valid_YearMonth,
      pack units_DateLike_valid_MonthDay
    ]
  where
    pack ::
      (a :| '[Year, Month, Day, YearMonthDay, YearMonth, MonthDay]) =>
      [(Text, a)] ->
      [(Text, Date)]
    pack = map (second (Date . Vary.from))

units_Date_invalidSemantics :: [Text]
units_Date_invalidSemantics =
  concat
    [ units_DateLike_invalidSemantics_Year,
      units_DateLike_invalidSemantics_Month,
      units_DateLike_invalidSemantics_Day,
      units_DateLike_invalidSemantics_YearMonthDay,
      units_DateLike_invalidSemantics_YearMonth,
      units_DateLike_invalidSemantics_MonthDay
    ]

units_Date_invalidSyntax :: [Text]
units_Date_invalidSyntax =
  concat
    [ units_Date_invalidSyntax_Year,
      units_Date_invalidSyntax_Month,
      units_Date_invalidSyntax_Day,
      units_Date_invalidSyntax_YearMonthDay,
      units_Date_invalidSyntax_YearMonth,
      units_Date_invalidSyntax_MonthDay
    ]

-- See also: units_Year_invalidSyntax
units_Date_invalidSyntax_Year :: [Text]
units_Date_invalidSyntax_Year =
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

-- See also: units_Month_invalidSyntax
units_Date_invalidSyntax_Month :: [Text]
units_Date_invalidSyntax_Month =
  concat
    [ -- incorrect number of digits
      ["--1", "--001"],
      ["--7", "--007"],
      ["--010"],
      -- too small/large numbers
      ["--0", "--000", "--013", "--020"],
      -- invalid number formats
      ["--1e1", "--10.0"],
      -- invalid characters
      ["--a", "--1a", "--a1"],
      -- leading or trailing whitespace
      [" --07", "\n--07", "\r\n--07", "--07 ", "--07\n", "--07\r\n"]
    ]

-- See also: units_Day_invalidSyntax
units_Date_invalidSyntax_Day :: [Text]
units_Date_invalidSyntax_Day =
  concat
    [ -- incorrect number of digits
      ["---1", "---001", "---0001"],
      ["---7", "---007", "---0007"],
      ["---030", "---0030"],
      ["---031", "---0031"],
      -- too small/large numbers
      ["---0", "---000", "---0000"],
      ["---032", "---0032"],
      ["---099", "---0099"],
      -- invalid number formats
      ["---1e1", "---10.0"],
      -- invalid characters
      ["---a", "---1a", "---a1"],
      -- leading or trailing whitespace
      [" ---07", "\n---07", "\r\n---07", "---07 ", "---07\n", "---07\r\n"]
    ]

units_Date_invalidSyntax_YearMonthDay :: [Text]
units_Date_invalidSyntax_YearMonthDay =
  [ -- extra dashes
    "5317-0412",
    "531704-12",
    "5317-04-12",
    -- incorrect number of digits
    "053170412",
    -- leading or trailing whitespace
    " 53170412",
    "\n53170412",
    "\r\n53170412",
    "53170412 ",
    "53170412\n",
    "53170412\r\n"
  ]

units_Date_invalidSyntax_YearMonth :: [Text]
units_Date_invalidSyntax_YearMonth =
  [ -- missing dash
    "481007",
    -- incorrect number of digits
    "04810-07",
    "4810-007",
    -- leading or trailing whitespace
    " 4810-007",
    "\n4810-007",
    "\r\n4810-007",
    "4810-007 ",
    "4810-007\n",
    "4810-007\r\n"
  ]

units_Date_invalidSyntax_MonthDay :: [Text]
units_Date_invalidSyntax_MonthDay =
  [ -- extra dash
    "--07-12",
    -- leading or trailing whitespace
    " --0712",
    "\n--0712",
    "\r\n--0712",
    "--0712 ",
    "--0712\n",
    "--0712\r\n"
  ]

--
-- DateList
--
test_DateList :: TestTree
test_DateList =
  testGroup
    "DateList"
    [ test_DateList_parse,
      test_DateList_serialize
    ]

test_DateList_parse :: TestTree
test_DateList_parse =
  testGroup
    "parse"
    [ testGroup
        "unit"
        [ testParseValid units_DateList_valid,
          testParseInvalidSemantics
            (Proxy @DateList)
            units_DateList_invalidSemantics,
          testParseInvalidSyntax (Proxy @DateList) units_DateList_invalidSyntax
        ]
    ]

test_DateList_serialize :: TestTree
test_DateList_serialize =
  testGroup
    "serialize"
    [ testSerialize "unit" units_DateList_valid
    ]

units_DateList_valid :: [(Text, DateList)]
units_DateList_valid =
  concat
    [ -- singletons
      map
        (second (List . NonEmpty.singleton))
        [ ("7316", date (y 7316)),
          ("--06", date (m 06)),
          ("---19", date (d 19)),
          ("43100327", date (ymd 4310 03 27)),
          ("1482-02", date (ym 1482 02)),
          ("--1204", date (md 12 04))
        ],
      -- pairs
      map
        (second List)
        [ ( "7316,43100327",
            date (y 7316) :| [date (ymd 4310 03 27)]
          ),
          ( "--06,1482-02",
            date (m 06) :| [date (ym 1482 02)]
          ),
          ( "---19,--1204",
            date (d 19) :| [date (md 12 04)]
          )
        ],
      -- triples
      map
        (second List)
        [ ( "7316,---19,1482-02",
            date (y 7316) :| [date (d 19), date (ym 1482 02)]
          ),
          ( "--06,43100327,--1204",
            date (m 06) :| [date (ymd 4310 03 27), date (md 12 04)]
          )
        ],
      -- combined
      [ ( "7316,--06,---19,43100327,1482-02,--1204",
          List
            ( date (y 7316)
                :| [ date (m 06),
                     date (d 19),
                     date (ymd 4310 03 27),
                     date (ym 1482 02),
                     date (md 12 04)
                   ]
            )
        )
      ],
      -- duplicates
      map
        (second List)
        [ ( "7316,7316",
            date (y 7316) :| [date (y 7316)]
          ),
          ( "--06,1482-02,--06",
            date (m 06) :| [date (ym 1482 02), date (m 06)]
          ),
          ( "---19,---19,---19",
            date (d 19) :| [date (d 19), date (d 19)]
          )
        ]
    ]

units_DateList_invalidSemantics :: [Text]
units_DateList_invalidSemantics =
  [ -- invalid dates
    "19700229",
    "--0431",
    "7316,19700229,---19"
  ]

units_DateList_invalidSyntax :: [Text]
units_DateList_invalidSyntax =
  concat
    [ -- extraneous leading/trailing whitespace
      [ " ---19,7316",
        "\n---19,7316",
        "\r\n---19,7316",
        "---19,7316 ",
        "---19,7316\n",
        "---19,7316\r\n"
      ],
      -- extraneous whitespace between entries
      [ "7316, --06",
        "7316,\n--06",
        "7316,\r\n--06",
        "7316,--06, ---19",
        "7316,--06,\n---19",
        "7316,--06,\r\n---19"
      ],
      -- empty strings/extraneous leading or trailing commas
      [ "",
        ",",
        ",,",
        "7316,",
        ",7316",
        ",7316,---19",
        "7316,---19,"
      ],
      -- invalid dates
      [ "316",
        "316,--06",
        "--06,316"
      ]
    ]

--
-- DateLike
--

-- See also: units_Year_valid
units_DateLike_valid_Year :: [(Text, Year)]
units_DateLike_valid_Year =
  [("0000", y 0000), ("6812", y 6812), ("9999", y 9999)]

-- See also: units_Month_valid
units_DateLike_valid_Month :: [(Text, Month)]
units_DateLike_valid_Month =
  [("--01", m 01), ("--05", m 05), ("--10", m 10), ("--12", m 12)]

-- See also: units_Day_valid
units_DateLike_valid_Day :: [(Text, Day)]
units_DateLike_valid_Day =
  [("---01", d 01), ("---15", d 15), ("---30", d 30), ("---31", d 31)]

-- See also: units_YearMonthDay_valid
units_DateLike_valid_YearMonthDay :: [(Text, YearMonthDay)]
units_DateLike_valid_YearMonthDay =
  [ -- bounds
    ("00000101", ymd 0000 01 01),
    ("99991231", ymd 9999 12 31),
    -- vary year
    ("00000412", ymd 0000 04 12),
    ("53170412", ymd 5317 04 12),
    ("99990412", ymd 9999 04 12),
    -- vary month
    ("47710112", ymd 4771 01 12),
    ("47710712", ymd 4771 07 12),
    ("47711212", ymd 4771 12 12),
    -- vary day
    ("39090801", ymd 3909 08 01),
    ("39090817", ymd 3909 08 17),
    ("39090831", ymd 3909 08 31),
    -- max day
    ("53170131", ymd 5317 01 31),
    ("53170228", ymd 5317 02 28),
    ("53170331", ymd 5317 03 31),
    ("53170430", ymd 5317 04 30),
    ("53170531", ymd 5317 05 31),
    ("53170630", ymd 5317 06 30),
    ("53170731", ymd 5317 07 31),
    ("53170831", ymd 5317 08 31),
    ("53170930", ymd 5317 09 30),
    ("53171031", ymd 5317 10 31),
    ("53171130", ymd 5317 11 30),
    ("53171231", ymd 5317 12 31),
    -- non leap years (not multiples of 4)
    ("00010228", ymd 0001 02 28),
    ("49220228", ymd 4922 02 28),
    ("99990228", ymd 9999 02 28),
    -- leap years (multiples of 4 but not of 100)
    ("00040228", ymd 0004 02 28),
    ("00040229", ymd 0004 02 29),
    ("67840228", ymd 6784 02 28),
    ("67840229", ymd 6784 02 29),
    ("99960228", ymd 9996 02 28),
    ("99960229", ymd 9996 02 29),
    -- non leap years (multiples of 100 but not of 400)
    ("01000228", ymd 0100 02 28),
    ("67000228", ymd 6700 02 28),
    ("99000228", ymd 9900 02 28),
    -- leap years (multiples of 400)
    ("00000228", ymd 0000 02 28),
    ("00000229", ymd 0000 02 29),
    ("68000228", ymd 6800 02 28),
    ("68000229", ymd 6800 02 29),
    ("96000228", ymd 9600 02 28),
    ("96000229", ymd 9600 02 29)
  ]

units_DateLike_valid_YearMonth :: [(Text, YearMonth)]
units_DateLike_valid_YearMonth =
  [ -- bounds
    ("0000-01", ym 0000 01),
    ("9999-12", ym 9999 12),
    -- vary year
    ("0000-07", ym 0000 07),
    ("4810-07", ym 4810 07),
    ("9999-07", ym 9999 07),
    -- vary month
    ("4810-01", ym 4810 01),
    ("4810-07", ym 4810 07),
    ("4810-12", ym 4810 12)
  ]

-- See also: units_MonthDay_valid
units_DateLike_valid_MonthDay :: [(Text, MonthDay)]
units_DateLike_valid_MonthDay =
  [ -- bounds
    ("--0101", md 01 01),
    ("--1231", md 12 31),
    -- vary month
    ("--0112", md 01 12),
    ("--0712", md 07 12),
    ("--1212", md 12 12),
    -- vary day
    ("--0701", md 07 01),
    ("--0712", md 07 12),
    ("--0731", md 07 31),
    -- max day
    ("--0131", md 01 31),
    ("--0228", md 02 28),
    ("--0229", md 02 29), -- February includes leap day
    ("--0331", md 03 31),
    ("--0430", md 04 30),
    ("--0531", md 05 31),
    ("--0630", md 06 30),
    ("--0731", md 07 31),
    ("--0831", md 08 31),
    ("--0930", md 09 30),
    ("--1031", md 10 31),
    ("--1130", md 11 30),
    ("--1231", md 12 31)
  ]

-- all syntactically valid years (yyyy) are also semantically valid
units_DateLike_invalidSemantics_Year :: [Text]
units_DateLike_invalidSemantics_Year = []

-- See also: units_Month_invalidSemantics
units_DateLike_invalidSemantics_Month :: [Text]
units_DateLike_invalidSemantics_Month =
  ["--00", "--13", "--14", "--15", "--20", "--99"]

-- See also: units_Day_invalidSemantics
units_DateLike_invalidSemantics_Day :: [Text]
units_DateLike_invalidSemantics_Day =
  ["---00", "---32", "---33", "---34", "---50", "---99"]

-- See also: units_YearMonthDay_invalid
units_DateLike_invalidSemantics_YearMonthDay :: [Text]
units_DateLike_invalidSemantics_YearMonthDay =
  [ -- beyond max day
    "53170229",
    "53170230",
    "53170231",
    "53170431",
    "53170631",
    "53170931",
    "53171131",
    -- non leap years (not multiples of 4)
    "00010229",
    "00010230",
    "00010231",
    "49220229",
    "49220230",
    "49220231",
    "99990229",
    "99990230",
    "99990231",
    -- leap years (multiples of 4 but not of 100)
    "00040230",
    "00040231",
    "67840230",
    "67840231",
    "99960230",
    "99960231",
    -- non leap years (multiples of 100 but not of 400)
    "01000229",
    "01000230",
    "01000231",
    "67000229",
    "67000230",
    "67000231",
    "99000229",
    "99000230",
    "99000231",
    -- leap years (multiples of 400)
    "00000230",
    "00000231",
    "68000230",
    "68000231",
    "96000230",
    "96000231",
    -- invalid month numbers
    "53171312",
    "53171412",
    "53171512",
    "53172012",
    "53179912",
    -- invalid day numbers
    "53170732",
    "53170733",
    "53170734",
    "53170750",
    "53170799"
  ]

units_DateLike_invalidSemantics_YearMonth :: [Text]
units_DateLike_invalidSemantics_YearMonth =
  [ -- invalid month numbers
    "5317-00",
    "5317-13",
    "5317-14",
    "5317-15",
    "5317-20",
    "5317-99"
  ]

-- See also: units_MonthDay_invalid
units_DateLike_invalidSemantics_MonthDay :: [Text]
units_DateLike_invalidSemantics_MonthDay =
  [ -- beyond max day
    "--0230",
    "--0231",
    "--0431",
    "--0631",
    "--0931",
    "--1131",
    -- invalid month numbers
    "--1312",
    "--1412",
    "--1512",
    "--2012",
    "--9912",
    -- invalid day numbers
    "--0532",
    "--0533",
    "--0534",
    "--0550",
    "--0599"
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

-- Potential dates
universe_Date_Year :: [Text]
universe_Date_Year = universe_Year

universe_Date_Month :: [Text]
universe_Date_Month = map ("--" <>) universe_Month

universe_Date_Day :: [Text]
universe_Date_Day = map ("---" <>) universe_Day

universe_Date_YearMonthDay :: [Text]
universe_Date_YearMonthDay = do
  year <- universe_Year
  month <- universe_Month
  day <- universe_Day

  pure (year <> month <> day)

universe_Date_YearMonth :: [Text]
universe_Date_YearMonth = do
  year <- universe_Year
  month <- universe_Month

  pure (year <> "-" <> month)

universe_Date_MonthDay :: [Text]
universe_Date_MonthDay = do
  month <- universe_Month
  day <- universe_Day

  pure ("--" <> month <> day)

-- Valid dates
exhaustive_Date_valid_Year :: [(Text, Date)]
exhaustive_Date_valid_Year = map (second date) exhaustive_Year_valid

exhaustive_Date_valid_Month :: [(Text, Date)]
exhaustive_Date_valid_Month =
  let mkDateMonth :: Text -> Month -> (Text, Date)
      mkDateMonth text month = ("--" <> text, date month)
   in map (uncurry mkDateMonth) exhaustive_Month_valid

exhaustive_Date_valid_Day :: [(Text, Date)]
exhaustive_Date_valid_Day =
  let mkDateMonth :: Text -> Day -> (Text, Date)
      mkDateMonth text day = ("---" <> text, date day)
   in map (uncurry mkDateMonth) exhaustive_Day_valid

exhaustive_Date_valid_YearMonthDay :: [(Text, Date)]
exhaustive_Date_valid_YearMonthDay = do
  (yearText, year) <- exhaustive_Year_valid
  (monthText, month) <- exhaustive_Month_valid
  (dayText, day) <- exhaustive_Day_valid
  yearMonthDay <- maybeToList (timeMkYearMonthDay year month day)
  let text = yearText <> monthText <> dayText
  pure (text, date yearMonthDay)

exhaustive_Date_valid_YearMonth :: [(Text, Date)]
exhaustive_Date_valid_YearMonth = do
  (yearText, year) <- exhaustive_Year_valid
  (monthText, month) <- exhaustive_Month_valid
  let yearMonth = YearMonth year month
  let text = yearText <> "-" <> monthText
  pure (text, date yearMonth)

exhaustive_Date_valid_MonthDay :: [(Text, Date)]
exhaustive_Date_valid_MonthDay = do
  (monthText, month) <- exhaustive_Month_valid
  (dayText, day) <- exhaustive_Day_valid
  monthDay <- maybeToList (timeMkMonthDay month day)
  let text = "--" <> monthText <> dayText
  pure (text, date monthDay)

exhaustive_Date_valid :: [(Text, Date)]
exhaustive_Date_valid =
  concat
    [ exhaustive_Date_valid_Year,
      exhaustive_Date_valid_Month,
      exhaustive_Date_valid_Day,
      exhaustive_Date_valid_YearMonthDay,
      exhaustive_Date_valid_YearMonth,
      exhaustive_Date_valid_MonthDay
    ]

-- Invalid dates
exhaustive_Date_invalid_Year :: [Text]
exhaustive_Date_invalid_Year =
  universe_Date_Year `minus` map fst exhaustive_Date_valid_Year

exhaustive_Date_invalid_Month :: [Text]
exhaustive_Date_invalid_Month =
  universe_Date_Month `minus` map fst exhaustive_Date_valid_Month

exhaustive_Date_invalid_Day :: [Text]
exhaustive_Date_invalid_Day =
  universe_Date_Day `minus` map fst exhaustive_Date_valid_Day

exhaustive_Date_invalid_YearMonthDay :: [Text]
exhaustive_Date_invalid_YearMonthDay =
  universe_Date_YearMonthDay `minus` map fst exhaustive_Date_valid_YearMonthDay

exhaustive_Date_invalid_YearMonth :: [Text]
exhaustive_Date_invalid_YearMonth =
  universe_Date_YearMonth `minus` map fst exhaustive_Date_valid_YearMonth

exhaustive_Date_invalid_MonthDay :: [Text]
exhaustive_Date_invalid_MonthDay =
  universe_Date_MonthDay `minus` map fst exhaustive_Date_valid_MonthDay

exhaustive_Date_invalid :: [Text]
exhaustive_Date_invalid =
  concat
    [ exhaustive_Date_invalid_YearMonthDay,
      exhaustive_Date_invalid_Year,
      exhaustive_Date_invalid_Month,
      exhaustive_Date_invalid_Day,
      exhaustive_Date_invalid_YearMonth,
      exhaustive_Date_invalid_MonthDay
    ]

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

------------------------------------
-- Functions with the `time` library
------------------------------------
timeMkMonthDay :: Month -> Day -> Maybe MonthDay
timeMkMonthDay month day =
  let isValid =
        isJust $
          monthAndDayToDayOfYearValid
            True
            (toTimeMonth month)
            (toTimeDay day)
   in if isValid
        then Just (MonthDay month day)
        else Nothing

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

date ::
  (a :| '[Year, Month, Day, YearMonthDay, YearMonth, MonthDay]) =>
  a ->
  Date
date = Date . Vary.from
