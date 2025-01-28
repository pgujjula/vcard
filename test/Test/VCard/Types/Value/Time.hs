-- SPDX-FileCopyrightText: Copyright Preetham Gujjula
-- SPDX-License-Identifier: BSD-3-Clause
{-# LANGUAGE CPP #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module Test.VCard.Types.Value.Time (tests) where

#if !MIN_VERSION_base(4,18,0)
import Control.Applicative (liftA2)
#endif
import Control.Monad (forM_, replicateM)
import Data.Bifunctor (bimap, second)
import Data.Finite (finite, finites)
import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.List.NonEmpty qualified as NonEmpty
import Data.List.Ordered (minus)
import Data.Proxy (Proxy (..))
import Data.Text (Text)
import Data.Text qualified as Text
import Test.Tasty (TestName, TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))
import TextShow (showt)
import VCard.Parse (HasParser, parse)
import VCard.Serialize (HasSerializer, serialize)
import VCard.Types.Value.List (List (..))
import VCard.Types.Value.Time
  ( Hour (..),
    HourMinute (..),
    HourMinuteSecond (..),
    LocalTime (..),
    Minute (..),
    MinuteSecond (..),
    Second (..),
    Sign (..),
    Time (..),
    TimeList,
    Zone (..),
  )
import Vary ((:|))
import Vary qualified

{-# ANN module ("HLint: ignore Use camelCase" :: String) #-}

tests :: TestTree
tests =
  testGroup
    "Time"
    [ test_Hour,
      test_Minute,
      test_Second,
      test_HourMinuteSecond,
      test_HourMinute,
      test_MinuteSecond,
      test_LocalTime,
      test_Time,
      test_TimeList,
      test_Sign,
      test_Zone
    ]

--
-- Hour
--
test_Hour :: TestTree
test_Hour =
  testGroup
    "Hour"
    [ test_Hour_parse,
      test_Hour_serialize,
      test_Hour_bounds
    ]

test_Hour_parse :: TestTree
test_Hour_parse =
  testGroup
    "parse"
    [ testGroup
        "unit"
        [ testParseValid units_Hour_valid,
          testParseInvalidSemantics (Proxy @Hour) units_Hour_invalidSemantics,
          testParseInvalidSyntax (Proxy @Hour) units_Hour_invalidSyntax
        ],
      testGroup
        "exhaustive"
        [ testParseValid exhaustive_Hour_valid,
          testParseInvalidSemantics (Proxy @Hour) exhaustive_Hour_invalid
        ]
    ]

test_Hour_serialize :: TestTree
test_Hour_serialize =
  testGroup
    "serialize"
    [ testSerialize "unit" units_Hour_valid,
      testSerialize "exhaustive" exhaustive_Hour_valid
    ]

test_Hour_bounds :: TestTree
test_Hour_bounds = testBounds (h 00, h 23)

-- See also: units_LocalTime_valid_Hour
units_Hour_valid :: [(Text, Hour)]
units_Hour_valid =
  [ ("00", Hour 00),
    ("02", Hour 02),
    ("15", Hour 15),
    ("23", Hour 23)
  ]

-- See also: units_LocalTime_invalidSemantics_Hour
units_Hour_invalidSemantics :: [Text]
units_Hour_invalidSemantics =
  ["24", "25", "26", "50", "99"]

-- See also: units_LocalTime_invalidSyntax_Hour
units_Hour_invalidSyntax :: [Text]
units_Hour_invalidSyntax =
  concat
    [ -- incorrect number of digits
      ["1", "001", "0001"],
      ["7", "007", "0007"],
      ["020", "0020"],
      -- negative numbers
      ["-1", "-01", "-001", "-0001"],
      ["-21", "-021", "-0021", "-00021"],
      -- too large numbers
      ["024", "0024"],
      ["050", "0050"],
      -- invalid number formats
      ["1e1", "20.0"],
      -- invalid characters
      ["a", "1a", "a1"],
      -- leading or trailing whitespace
      [" 07", "\n07", "\r\n07", "07 ", "07\n", "07\r\n"]
    ]

--
-- Minute
--
test_Minute :: TestTree
test_Minute =
  testGroup
    "Minute"
    [ test_Minute_parse,
      test_Minute_serialize,
      test_Minute_bounds
    ]

test_Minute_parse :: TestTree
test_Minute_parse =
  testGroup
    "parse"
    [ testGroup
        "unit"
        [ testParseValid units_Minute_valid,
          testParseInvalidSemantics
            (Proxy @Minute)
            units_Minute_invalidSemantics,
          testParseInvalidSyntax (Proxy @Minute) units_Minute_invalidSyntax
        ],
      testGroup
        "exhaustive"
        [ testParseValid exhaustive_Minute_valid,
          testParseInvalidSemantics (Proxy @Minute) exhaustive_Minute_invalid
        ]
    ]

test_Minute_serialize :: TestTree
test_Minute_serialize =
  testGroup
    "serialize"
    [ testSerialize "unit" units_Minute_valid,
      testSerialize "exhaustive" exhaustive_Minute_valid
    ]

test_Minute_bounds :: TestTree
test_Minute_bounds = testBounds (m 00, m 59)

-- See also: units_LocalTime_valid_Minute
units_Minute_valid :: [(Text, Minute)]
units_Minute_valid =
  [ ("00", Minute 00),
    ("08", Minute 08),
    ("45", Minute 45),
    ("59", Minute 59)
  ]

-- See also: units_LocalTime_invalidSemantics_Minute
units_Minute_invalidSemantics :: [Text]
units_Minute_invalidSemantics =
  ["60", "61", "62", "63", "75", "99"]

-- See also: units_LocalTime_invalidSyntax_Minute
units_Minute_invalidSyntax :: [Text]
units_Minute_invalidSyntax =
  concat
    [ -- incorrect number of digits
      ["1", "001", "0001"],
      ["7", "007", "0007"],
      ["020", "0020"],
      -- negative numbers
      ["-1", "-01", "-001", "-0001"],
      ["-21", "-021", "-0021", "-00021"],
      -- too large numbers
      ["060", "0060"],
      ["075", "0075"],
      -- invalid number formats
      ["1e1", "20.0"],
      -- invalid characters
      ["a", "1a", "a1"],
      -- leading or trailing whitespace
      [" 07", "\n07", "\r\n07", "07 ", "07\n", "07\r\n"]
    ]

--
-- Second
--
test_Second :: TestTree
test_Second =
  testGroup
    "Second"
    [ test_Second_parse,
      test_Second_serialize,
      test_Second_bounds
    ]

test_Second_parse :: TestTree
test_Second_parse =
  testGroup
    "parse"
    [ testGroup
        "unit"
        [ testParseValid units_Second_valid,
          testParseInvalidSemantics
            (Proxy @Second)
            units_Second_invalidSemantics,
          testParseInvalidSyntax (Proxy @Second) units_Second_invalidSyntax
        ],
      testGroup
        "exhaustive"
        [ testParseValid exhaustive_Second_valid,
          testParseInvalidSemantics (Proxy @Second) exhaustive_Second_invalid
        ]
    ]

test_Second_serialize :: TestTree
test_Second_serialize =
  testGroup
    "serialize"
    [ testSerialize "unit" units_Second_valid,
      testSerialize "exhaustive" exhaustive_Second_valid
    ]

test_Second_bounds :: TestTree
test_Second_bounds = testBounds (s 00, s 60)

-- See also: units_LocalTime_valid_Second
units_Second_valid :: [(Text, Second)]
units_Second_valid =
  [ ("00", Second 00),
    ("08", Second 08),
    ("45", Second 45),
    ("59", Second 59),
    ("60", Second 60)
  ]

-- See also: units_LocalTime_invalidSemantics_Second
units_Second_invalidSemantics :: [Text]
units_Second_invalidSemantics =
  ["61", "62", "63", "75", "99"]

-- See also: units_LocalTime_invalidSyntax_Second
units_Second_invalidSyntax :: [Text]
units_Second_invalidSyntax =
  concat
    [ -- incorrect number of digits
      ["1", "001", "0001"],
      ["7", "007", "0007"],
      ["020", "0020"],
      -- negative numbers
      ["-1", "-01", "-001", "-0001"],
      ["-21", "-021", "-0021", "-00021"],
      -- too large numbers
      ["061", "0061"],
      ["075", "0075"],
      -- invalid number formats
      ["1e1", "20.0"],
      -- invalid characters
      ["a", "1a", "a1"],
      -- leading or trailing whitespace
      [" 07", "\n07", "\r\n07", "07 ", "07\n", "07\r\n"]
    ]

--
-- HourMinuteSecond
--
test_HourMinuteSecond :: TestTree
test_HourMinuteSecond =
  testGroup
    "HourMinuteSecond"
    [ test_HourMinuteSecond_bounds
    ]

test_HourMinuteSecond_bounds :: TestTree
test_HourMinuteSecond_bounds = testBounds (hms 00 00 00, hms 23 59 60)

--
-- HourMinute
--
test_HourMinute :: TestTree
test_HourMinute =
  testGroup
    "HourMinute"
    [ test_HourMinute_bounds
    ]

test_HourMinute_bounds :: TestTree
test_HourMinute_bounds = testBounds (hm 00 00, hm 23 59)

--
-- MinuteSecond
--
test_MinuteSecond :: TestTree
test_MinuteSecond =
  testGroup
    "MinuteSecond"
    [ test_MinuteSecond_bounds
    ]

test_MinuteSecond_bounds :: TestTree
test_MinuteSecond_bounds = testBounds (ms 00 00, ms 59 60)

--
-- LocalTime
--

test_LocalTime :: TestTree
test_LocalTime =
  testGroup
    "LocalTime"
    [ test_LocalTime_parse,
      test_LocalTime_serialize
    ]

test_LocalTime_parse :: TestTree
test_LocalTime_parse =
  testGroup
    "parse"
    [ testGroup
        "unit"
        [ testParseValid units_LocalTime_valid,
          testParseInvalidSemantics
            (Proxy @LocalTime)
            units_LocalTime_invalidSemantics,
          testParseInvalidSyntax
            (Proxy @LocalTime)
            units_LocalTime_invalidSyntax
        ],
      testGroup
        "exhaustive"
        [ testParseValid exhaustive_LocalTime_valid,
          testParseInvalidSemantics
            (Proxy @LocalTime)
            exhaustive_LocalTime_invalid
        ]
    ]

test_LocalTime_serialize :: TestTree
test_LocalTime_serialize =
  testGroup
    "serialize"
    [ testSerialize "unit" units_LocalTime_valid,
      testSerialize "exhaustive" exhaustive_LocalTime_valid
    ]

units_LocalTime_valid :: [(Text, LocalTime)]
units_LocalTime_valid =
  concat
    [ pack units_LocalTimeLike_valid_Hour,
      pack units_LocalTimeLike_valid_Minute,
      pack units_LocalTimeLike_valid_Second,
      pack units_LocalTimeLike_valid_HourMinuteSecond,
      pack units_LocalTimeLike_valid_HourMinute,
      pack units_LocalTimeLike_valid_MinuteSecond
    ]
  where
    pack ::
      (a :| '[Hour, Minute, Second, HourMinuteSecond, HourMinute, MinuteSecond]) =>
      [(Text, a)] ->
      [(Text, LocalTime)]
    pack = map (second localTime)

units_LocalTime_invalidSemantics :: [Text]
units_LocalTime_invalidSemantics =
  concat
    [ units_LocalTimeLike_invalidSemantics_Hour,
      units_LocalTimeLike_invalidSemantics_Minute,
      units_LocalTimeLike_invalidSemantics_Second,
      units_LocalTimeLike_invalidSemantics_HourMinuteSecond,
      units_LocalTimeLike_invalidSemantics_HourMinute,
      units_LocalTimeLike_invalidSemantics_MinuteSecond
    ]

units_LocalTime_invalidSyntax :: [Text]
units_LocalTime_invalidSyntax =
  concat
    [ units_LocalTimeLike_invalidSyntax_Hour,
      units_LocalTimeLike_invalidSyntax_Minute,
      units_LocalTimeLike_invalidSyntax_Second,
      units_LocalTimeLike_invalidSyntax_HourMinuteSecond,
      units_LocalTimeLike_invalidSyntax_HourMinute,
      units_LocalTimeLike_invalidSyntax_MinuteSecond
    ]

--
-- LocalTimeLike
--

-- See also: units_Hour_valid
units_LocalTimeLike_valid_Hour :: [(Text, Hour)]
units_LocalTimeLike_valid_Hour =
  [("00", h 00), ("02", h 02), ("15", h 15), ("23", h 23)]

-- See also: units_Minute_valid
units_LocalTimeLike_valid_Minute :: [(Text, Minute)]
units_LocalTimeLike_valid_Minute =
  [("-00", m 00), ("-08", m 08), ("-45", m 45), ("-59", m 59)]

-- See also: units_Second_valid
units_LocalTimeLike_valid_Second :: [(Text, Second)]
units_LocalTimeLike_valid_Second =
  [ ("--00", s 00),
    ("--08", s 08),
    ("--45", s 45),
    ("--59", s 59),
    ("--60", s 60)
  ]

units_LocalTimeLike_valid_HourMinuteSecond :: [(Text, HourMinuteSecond)]
units_LocalTimeLike_valid_HourMinuteSecond =
  [ -- bounds
    ("000000", hms 00 00 00),
    ("235960", hms 23 59 60),
    -- vary hour
    ("001739", hms 00 17 39),
    ("081739", hms 08 17 39),
    ("231739", hms 23 17 39),
    -- vary minute
    ("130054", hms 13 00 54),
    ("132954", hms 13 29 54),
    ("135954", hms 13 59 54),
    -- vary second
    ("013700", hms 01 37 00),
    ("013726", hms 01 37 26),
    ("013760", hms 01 37 60)
  ]

units_LocalTimeLike_valid_HourMinute :: [(Text, HourMinute)]
units_LocalTimeLike_valid_HourMinute =
  [ -- bounds
    ("0000", hm 00 00),
    ("2359", hm 23 59),
    -- vary hour
    ("0017", hm 00 17),
    ("0817", hm 08 17),
    ("2317", hm 23 17),
    -- vary minute
    ("1300", hm 13 00),
    ("1329", hm 13 29),
    ("1359", hm 13 59)
  ]

units_LocalTimeLike_valid_MinuteSecond :: [(Text, MinuteSecond)]
units_LocalTimeLike_valid_MinuteSecond =
  [ -- bounds
    ("-0000", ms 00 00),
    ("-5960", ms 59 60),
    -- vary minute
    ("-0054", ms 00 54),
    ("-2954", ms 29 54),
    ("-5954", ms 59 54),
    -- vary second
    ("-3700", ms 37 00),
    ("-3726", ms 37 26),
    ("-3760", ms 37 60)
  ]

-- See also: units_Hour_invalidSemantics
units_LocalTimeLike_invalidSemantics_Hour :: [Text]
units_LocalTimeLike_invalidSemantics_Hour =
  ["24", "25", "26", "50", "99"]

-- See also: units_Minute_invalidSemantics
units_LocalTimeLike_invalidSemantics_Minute :: [Text]
units_LocalTimeLike_invalidSemantics_Minute =
  ["-60", "-61", "-62", "-63", "-75", "-99"]

-- See also: units_Second_invalidSemantics
units_LocalTimeLike_invalidSemantics_Second :: [Text]
units_LocalTimeLike_invalidSemantics_Second =
  ["--61", "--62", "--63", "--75", "--99"]

units_LocalTimeLike_invalidSemantics_HourMinuteSecond :: [Text]
units_LocalTimeLike_invalidSemantics_HourMinuteSecond =
  [ -- invalid hours
    "240000",
    "241739",
    "251739",
    "261739",
    "501739",
    "991739",
    -- invalid minutes
    "006000",
    "136054",
    "136154",
    "136254",
    "136354",
    "137554",
    "139954",
    -- invalid seconds
    "000061",
    "013761",
    "013762",
    "013763",
    "013775",
    "013799"
  ]

units_LocalTimeLike_invalidSemantics_HourMinute :: [Text]
units_LocalTimeLike_invalidSemantics_HourMinute =
  [ -- invalid hours
    "2400",
    "2417",
    "2517",
    "2617",
    "5017",
    "9917",
    -- invalid minutes
    "0060",
    "1360",
    "1361",
    "1362",
    "1363",
    "1375",
    "1399"
  ]

units_LocalTimeLike_invalidSemantics_MinuteSecond :: [Text]
units_LocalTimeLike_invalidSemantics_MinuteSecond =
  [ -- invalid minutes
    "-6000",
    "-6054",
    "-6154",
    "-6254",
    "-6354",
    "-7554",
    "-9954",
    -- invalid seconds
    "-0061",
    "-3761",
    "-3762",
    "-3763",
    "-3775",
    "-3799"
  ]

-- See also: units_Hour_invalidSyntax
units_LocalTimeLike_invalidSyntax_Hour :: [Text]
units_LocalTimeLike_invalidSyntax_Hour =
  concat
    [ -- incorrect number of digits
      ["1", "001"],
      ["7", "007"],
      ["020"],
      -- too large numbers
      ["024"],
      ["050"],
      -- invalid number formats
      ["1e1", "20.0"],
      -- invalid characters
      ["a", "1a", "a1"],
      -- leading or trailing whitespace
      [" 07", "\n07", "\r\n07", "07 ", "07\n", "07\r\n"]
    ]

-- See also: units_Minute_invalidSyntax
units_LocalTimeLike_invalidSyntax_Minute :: [Text]
units_LocalTimeLike_invalidSyntax_Minute =
  concat
    [ -- incorrect number of digits
      ["-1", "-001"],
      ["-7", "-007"],
      ["-020"],
      -- too large numbers
      ["-060"],
      ["-075"],
      -- invalid number formats
      ["-1e1", "-20.0"],
      -- invalid characters
      ["-a", "-1a", "-a1"],
      -- leading or trailing whitespace
      [" -07", "\n-07", "\r\n-07", "-07 ", "-07\n", "-07\r\n"]
    ]

-- See also: units_Second_invalidSyntax
units_LocalTimeLike_invalidSyntax_Second :: [Text]
units_LocalTimeLike_invalidSyntax_Second =
  concat
    [ -- incorrect number of digits
      ["--1", "--001", "--0001"],
      ["--7", "--007", "--0007"],
      ["--020", "--0020"],
      -- too large numbers
      ["--061", "--0061"],
      ["--075", "--0075"],
      -- invalid number formats
      ["--1e1", "--20.0"],
      -- invalid characters
      ["--a", "--1a", "--a1"],
      -- leading or trailing whitespace
      [" --07", "\n--07", "\r\n--07", "--07 ", "--07\n", "--07\r\n"]
    ]

units_LocalTimeLike_invalidSyntax_HourMinuteSecond :: [Text]
units_LocalTimeLike_invalidSyntax_HourMinuteSecond =
  concat
    [ -- extra dashes
      [ "08-1739",
        "0817-39",
        "08-17-39"
      ],
      -- incorrect number of digits
      ["0081739"],
      -- leading or trailing whitespace
      [ " 081739",
        "\n081739",
        "\r\n081739",
        "081739 ",
        "081739\n",
        " 081739\r\n"
      ]
    ]

units_LocalTimeLike_invalidSyntax_HourMinute :: [Text]
units_LocalTimeLike_invalidSyntax_HourMinute =
  concat
    [ -- extra dash
      ["08-17", "0817-"],
      -- incorrect number of digits
      ["00817"],
      -- leading or trailing whitespace
      [" 0817", "\n0817", "\r\n0817", "0817 ", "0817\n", "0817\r\n"]
    ]

units_LocalTimeLike_invalidSyntax_MinuteSecond :: [Text]
units_LocalTimeLike_invalidSyntax_MinuteSecond =
  concat
    [ -- extra dash
      ["-29-54", "--2954"],
      -- incorrect number of digits
      ["-02954"],
      -- leading or trailing whitespace
      [" -2954", "\n-2954", "\r\n-2954", "-2954 ", "-2954\n", "-2954\r\n"]
    ]

--
-- Time
--
test_Time :: TestTree
test_Time =
  testGroup
    "Time"
    [ test_Time_parse,
      test_Time_serialize
    ]

test_Time_parse :: TestTree
test_Time_parse =
  testGroup
    "parse"
    [ testParseValid units_Time_valid,
      testParseInvalidSemantics (Proxy @Time) units_Time_invalidSemantics,
      testParseInvalidSyntax (Proxy @Time) units_Time_invalidSyntax
    ]

test_Time_serialize :: TestTree
test_Time_serialize = testSerialize "serialize" units_Time_valid

units_Time_valid :: [(Text, Time)]
units_Time_valid = units_Time_valid1 ++ units_Time_valid2

units_Time_valid1 :: [(Text, Time)]
units_Time_valid1 =
  [ --
    ("15", time (h 15) Nothing),
    ("15Z", time (h 15) (Just UTCDesignator)),
    ("15+15", time (h 15) (Just (UTCOffset Plus (h 15) Nothing))),
    ("15-1537", time (h 15) (Just (UTCOffset Minus (h 15) (Just (m 37))))),
    --
    ("-45", time (m 45) Nothing),
    ("-45Z", time (m 45) (Just UTCDesignator)),
    ("-45+15", time (m 45) (Just (UTCOffset Plus (h 15) Nothing))),
    ("-45-1537", time (m 45) (Just (UTCOffset Minus (h 15) (Just (m 37))))),
    --
    ("--08", time (s 08) Nothing),
    ("--08Z", time (s 08) (Just UTCDesignator)),
    ("--08+15", time (s 08) (Just (UTCOffset Plus (h 15) Nothing))),
    ("--08-1537", time (s 08) (Just (UTCOffset Minus (h 15) (Just (m 37))))),
    --
    ("081739", time (hms 08 17 39) Nothing),
    ("081739Z", time (hms 08 17 39) (Just UTCDesignator)),
    ("081739+15", time (hms 08 17 39) (Just (UTCOffset Plus (h 15) Nothing))),
    ( "081739-1537",
      time (hms 08 17 39) (Just (UTCOffset Minus (h 15) (Just (m 37))))
    ),
    --
    ("1329", time (hm 13 29) Nothing),
    ("1329Z", time (hm 13 29) (Just UTCDesignator)),
    ("1329+15", time (hm 13 29) (Just (UTCOffset Plus (h 15) Nothing))),
    ( "1329-1537",
      time (hm 13 29) (Just (UTCOffset Minus (h 15) (Just (m 37))))
    ),
    --
    ("-3726", time (ms 37 26) Nothing),
    ("-3726Z", time (ms 37 26) (Just UTCDesignator)),
    ("-3726+15", time (ms 37 26) (Just (UTCOffset Plus (h 15) Nothing))),
    ( "-3726-1537",
      time (ms 37 26) (Just (UTCOffset Minus (h 15) (Just (m 37))))
    )
  ]

units_Time_valid2 :: [(Text, Time)]
units_Time_valid2 = do
  (localTimeText, localTime') <- units_LocalTime_valid
  (zoneText, zone) <- ("", Nothing) : map (second Just) units_Zone_valid

  let timeText = localTimeText <> zoneText
      time' = Time localTime' zone

  pure (timeText, time')

units_Time_invalidSemantics :: [Text]
units_Time_invalidSemantics =
  concat
    [ -- invalid hour
      ["24", "240000+0000", "2429Z"],
      -- invalid minute
      ["-60", "-6000+00", "-6037Z"],
      -- invalid second
      ["--61", "--61+00", "--61Z"],
      -- invalid zone hour
      ["0000+2400", "-45+24"],
      -- invalid zone minute
      ["0000+0060", "-45+0460"]
    ]

units_Time_invalidSyntax :: [Text]
units_Time_invalidSyntax =
  [ -- strange constructions
    "081739Z1537",
    "0817391537",
    -- extraneous whitespace
    " 081739+1537",
    "\n081739+1537",
    "\r\n081739+1537",
    "081739+1537 ",
    "081739+1537\n",
    "081739+1537\r\n"
  ]

--
-- TimeList
--
test_TimeList :: TestTree
test_TimeList =
  testGroup
    "TimeList"
    [ test_TimeList_parse,
      test_TimeList_serialize
    ]

test_TimeList_parse :: TestTree
test_TimeList_parse =
  testGroup
    "parse"
    [ testParseValid units_TimeList_valid,
      testParseInvalidSemantics
        (Proxy @TimeList)
        units_TimeList_invalidSemantics,
      testParseInvalidSyntax (Proxy @TimeList) units_TimeList_invalidSyntax
    ]

test_TimeList_serialize :: TestTree
test_TimeList_serialize = testSerialize "serialize" units_TimeList_valid

units_TimeList_valid :: [(Text, TimeList)]
units_TimeList_valid =
  concat
    [ -- singletons
      map
        (second (List . NonEmpty.singleton))
        [ ("02", time (h 02) Nothing),
          ("-45Z", time (m 45) (Just UTCDesignator)),
          ("--15+11", time (s 15) (Just (UTCOffset Plus (h 11) Nothing))),
          ( "081739+0942",
            time (hms 08 17 39) (Just (UTCOffset Plus (h 09) (Just (m 42))))
          ),
          ("1329-11", time (hm 13 29) (Just (UTCOffset Minus (h 11) Nothing))),
          ( "-3726-0942",
            time (ms 37 26) (Just (UTCOffset Minus (h 09) (Just (m 42))))
          )
        ],
      -- pairs
      map
        (second List)
        [ ( "02,081739+0942",
            time (h 02) Nothing
              :| [ time
                     (hms 08 17 39)
                     (Just (UTCOffset Plus (h 09) (Just (m 42))))
                 ]
          ),
          ( "-45Z,1329-11",
            time (m 45) (Just UTCDesignator)
              :| [time (hm 13 29) (Just (UTCOffset Minus (h 11) Nothing))]
          ),
          ( "--15+11,-3726-0942",
            time (s 15) (Just (UTCOffset Plus (h 11) Nothing))
              :| [time (ms 37 26) (Just (UTCOffset Minus (h 09) (Just (m 42))))]
          )
        ],
      -- triples
      map
        (second List)
        [ ( "02,--15+11,1329-11",
            time (h 02) Nothing
              :| [ time (s 15) (Just (UTCOffset Plus (h 11) Nothing)),
                   time (hm 13 29) (Just (UTCOffset Minus (h 11) Nothing))
                 ]
          ),
          ( "-45Z,081739+0942,-3726-0942",
            time (m 45) (Just UTCDesignator)
              :| [ time
                     (hms 08 17 39)
                     (Just (UTCOffset Plus (h 09) (Just (m 42)))),
                   time (ms 37 26) (Just (UTCOffset Minus (h 09) (Just (m 42))))
                 ]
          )
        ],
      -- duplicates
      map
        (second List)
        [ ("02,02", time (h 02) Nothing :| [time (h 02) Nothing]),
          ( "-45Z,--15+11,-45Z",
            time (m 45) (Just UTCDesignator)
              :| [ time (s 15) (Just (UTCOffset Plus (h 11) Nothing)),
                   time (m 45) (Just UTCDesignator)
                 ]
          ),
          ( "1329-11,1329-11,1329-11",
            time (hm 13 29) (Just (UTCOffset Minus (h 11) Nothing))
              :| [ time (hm 13 29) (Just (UTCOffset Minus (h 11) Nothing)),
                   time (hm 13 29) (Just (UTCOffset Minus (h 11) Nothing))
                 ]
          )
        ]
    ]

units_TimeList_invalidSemantics :: [Text]
units_TimeList_invalidSemantics =
  [ -- invalid times
    "24",
    "--15+11,-60Z",
    "081739+0960,1329-11,-3726-0942"
  ]

units_TimeList_invalidSyntax :: [Text]
units_TimeList_invalidSyntax =
  concat
    [ -- leading/trailing whitespace
      [ " -45+15,--08Z",
        "\n-45+15,--08Z",
        "\r\n-45+15,--08Z",
        "-45+15,--08Z ",
        "-45+15,--08Z\n",
        "-45+15,--08Z\r\n"
      ],
      -- whitespace between entries
      [ "--08+15, 02,1329-11",
        "--08+15,\n02,1329-11",
        "--08+15,\r\n02,1329-11",
        "--08+15,02, 1329-11",
        "--08+15,02,\n1329-11",
        "--08+15,02,\r\n1329-11"
      ],
      -- empty strings/extraneous leading or trailing commas
      [ "",
        ",",
        ",,",
        "--08+15,",
        ",--08+15",
        ",--08+15,1329-11",
        "--08+15,1329-11,"
      ],
      -- invalid times
      [ "--8+15",
        "--8+15,1329-11",
        "1329-11,--8+15"
      ]
    ]

--
-- Sign
--
test_Sign :: TestTree
test_Sign =
  testGroup
    "Sign"
    [ test_Sign_parse,
      test_Sign_serialize,
      test_Sign_bounds
    ]

test_Sign_parse :: TestTree
test_Sign_parse =
  testGroup
    "parse"
    [ testParseValid units_Sign_valid,
      testParseInvalidSemantics (Proxy @Sign) units_Sign_invalidSemantics,
      testParseInvalidSyntax (Proxy @Sign) units_Sign_invalidSyntax
    ]

test_Sign_serialize :: TestTree
test_Sign_serialize =
  testSerialize "serialize" units_Sign_valid

test_Sign_bounds :: TestTree
test_Sign_bounds = testBounds (Minus, Plus)

units_Sign_valid :: [(Text, Sign)]
units_Sign_valid =
  [ ("+", Plus),
    ("-", Minus)
  ]

units_Sign_invalidSemantics :: [Text]
units_Sign_invalidSemantics =
  []

units_Sign_invalidSyntax :: [Text]
units_Sign_invalidSyntax =
  -- weird constructions
  ["++", "+-", "-+", "--"]
    -- leading or trailing whitespace
    ++ [" +", "\n+", "\r\n+", "+ ", "+\n", "+\r\n"]

--
-- Zone
--
test_Zone :: TestTree
test_Zone =
  testGroup
    "Zone"
    [ test_Zone_parse,
      test_Zone_serialize
    ]

test_Zone_parse :: TestTree
test_Zone_parse =
  testGroup
    "parse"
    [ testParseValid units_Zone_valid,
      testParseInvalidSemantics (Proxy @Zone) units_Zone_invalidSemantics,
      testParseInvalidSyntax (Proxy @Zone) units_Zone_invalidSyntax
    ]

test_Zone_serialize :: TestTree
test_Zone_serialize =
  testSerialize "serialize" units_Zone_valid

units_Zone_valid :: [(Text, Zone)]
units_Zone_valid =
  [ -- UTCDesignator
    ("Z", UTCDesignator),
    -- UTCOffset
    --   without minute
    ("-00", UTCOffset Minus (h 00) Nothing),
    ("-15", UTCOffset Minus (h 15) Nothing),
    ("-23", UTCOffset Minus (h 23) Nothing),
    ("+00", UTCOffset Plus (h 00) Nothing),
    ("+15", UTCOffset Plus (h 15) Nothing),
    ("+23", UTCOffset Plus (h 23) Nothing),
    --   with minute
    ("-0000", UTCOffset Minus (h 00) (Just (m 00))),
    ("-0037", UTCOffset Minus (h 00) (Just (m 37))),
    ("-0059", UTCOffset Minus (h 00) (Just (m 59))),
    --
    ("-1500", UTCOffset Minus (h 15) (Just (m 00))),
    ("-1537", UTCOffset Minus (h 15) (Just (m 37))),
    ("-1559", UTCOffset Minus (h 15) (Just (m 59))),
    --
    ("-2300", UTCOffset Minus (h 23) (Just (m 00))),
    ("-2337", UTCOffset Minus (h 23) (Just (m 37))),
    ("-2359", UTCOffset Minus (h 23) (Just (m 59))),
    --
    ("+0000", UTCOffset Plus (h 00) (Just (m 00))),
    ("+0037", UTCOffset Plus (h 00) (Just (m 37))),
    ("+0059", UTCOffset Plus (h 00) (Just (m 59))),
    --
    ("+1500", UTCOffset Plus (h 15) (Just (m 00))),
    ("+1537", UTCOffset Plus (h 15) (Just (m 37))),
    ("+1559", UTCOffset Plus (h 15) (Just (m 59))),
    --
    ("+2300", UTCOffset Plus (h 23) (Just (m 00))),
    ("+2337", UTCOffset Plus (h 23) (Just (m 37))),
    ("+2359", UTCOffset Plus (h 23) (Just (m 59)))
  ]

units_Zone_invalidSemantics :: [Text]
units_Zone_invalidSemantics =
  concat
    [ -- without minute, invalid hours
      ["-24", "-25", "-99", "+24", "+25", "+99"],
      -- with minute, invalid hours
      ["-2437", "-2537", "-2637", "-9937"],
      ["+2437", "+2537", "+2637", "+9937"],
      -- with minute, invalid minute
      ["-1560", "-1561", "-1562", "-1599"],
      ["+1560", "+1561", "+1562", "+1599"]
    ]

units_Zone_invalidSyntax :: [Text]
units_Zone_invalidSyntax =
  concat
    [ -- UTCDesignator
      ["z"],
      -- UTCOffset
      --   missing sign
      ["00", "15", "23"],
      --   incorrect number of digits
      ["-0", "-000", "-00000"],
      ["-1", "-121", "-12121"],
      ["+0", "+000", "+00000"],
      ["+1", "+121", "+12121"],
      --   leading or trailing whitespace
      [" +1537", "\n+1537", "\r\n+1537", "+1537 ", "+1537\n", "+1537\r\n"]
    ]

-- =========
-- UTILITIES
-- =========

-----------------------------------
-- Hour, Minute, Second enumeration
-----------------------------------

hours :: [Hour]
hours = map Hour finites

minutes :: [Minute]
minutes = map Minute finites

seconds :: [Second]
seconds = map Second finites

-- All `Text`s in the syntactic format of an `Hour` (hh), `Minute` (mm), or
-- `Second` (ss), but not necessarily semantically valid.
universe_Hour :: [Text]
universe_Hour = map Text.pack (replicateM 2 ['0' .. '9'])

universe_Minute :: [Text]
universe_Minute = map Text.pack (replicateM 2 ['0' .. '9'])

universe_Second :: [Text]
universe_Second = map Text.pack (replicateM 2 ['0' .. '9'])

-- All `Text`s that represent semantically valid `Hour`s, `Month`s, or `Second`s
-- paired with their parsed value.
exhaustive_Hour_valid :: [(Text, Hour)]
exhaustive_Hour_valid =
  let texts = map (Text.justifyRight 2 '0' . showt @Int) [0 .. 23]
   in zip texts hours

exhaustive_Minute_valid :: [(Text, Minute)]
exhaustive_Minute_valid =
  let texts = map (Text.justifyRight 2 '0' . showt @Int) [0 .. 60]
   in zip texts minutes

exhaustive_Second_valid :: [(Text, Second)]
exhaustive_Second_valid =
  let texts = map (Text.justifyRight 2 '0' . showt @Int) [0 .. 61]
   in zip texts seconds

-- `Text`s that fit the syntactic format of `Hour`, `Minute`, or `Second` but
-- are not semantically valid. Example: "30" is in exhaustive_Hour_invalid
exhaustive_Hour_invalid :: [Text]
exhaustive_Hour_invalid = universe_Hour `minus` map fst exhaustive_Hour_valid

exhaustive_Minute_invalid :: [Text]
exhaustive_Minute_invalid = universe_Minute `minus` map fst exhaustive_Minute_valid

exhaustive_Second_invalid :: [Text]
exhaustive_Second_invalid = universe_Second `minus` map fst exhaustive_Second_valid

------------------------
-- LocalTime enumeration
------------------------

-- Potential LocalTimes
universe_LocalTime_Hour :: [Text]
universe_LocalTime_Hour = universe_Hour

universe_LocalTime_Minute :: [Text]
universe_LocalTime_Minute = map ("-" <>) universe_Minute

universe_LocalTime_Second :: [Text]
universe_LocalTime_Second = map ("--" <>) universe_Second

universe_LocalTime_HourMinuteSecond :: [Text]
universe_LocalTime_HourMinuteSecond = do
  hour <- universe_Hour
  minute <- universe_Minute
  second' <- universe_Second

  pure (hour <> minute <> second')

universe_LocalTime_HourMinute :: [Text]
universe_LocalTime_HourMinute = liftA2 (<>) universe_Hour universe_Minute

universe_LocalTime_MinuteSecond :: [Text]
universe_LocalTime_MinuteSecond = do
  minute <- universe_Minute
  second' <- universe_Second

  pure ("-" <> minute <> second')

-- Valid LocalTimes
exhaustive_LocalTime_valid :: [(Text, LocalTime)]
exhaustive_LocalTime_valid =
  concat
    [ exhaustive_LocalTime_valid_Hour,
      exhaustive_LocalTime_valid_Minute,
      exhaustive_LocalTime_valid_Second,
      exhaustive_LocalTime_valid_HourMinuteSecond,
      exhaustive_LocalTime_valid_HourMinute,
      exhaustive_LocalTime_valid_MinuteSecond
    ]

exhaustive_LocalTime_valid_Hour :: [(Text, LocalTime)]
exhaustive_LocalTime_valid_Hour = map (second localTime) exhaustive_Hour_valid

exhaustive_LocalTime_valid_Minute :: [(Text, LocalTime)]
exhaustive_LocalTime_valid_Minute =
  map (bimap ("-" <>) localTime) exhaustive_Minute_valid

exhaustive_LocalTime_valid_Second :: [(Text, LocalTime)]
exhaustive_LocalTime_valid_Second =
  map (bimap ("--" <>) localTime) exhaustive_Second_valid

exhaustive_LocalTime_valid_HourMinuteSecond :: [(Text, LocalTime)]
exhaustive_LocalTime_valid_HourMinuteSecond = do
  (hourText, hour) <- exhaustive_Hour_valid
  (minuteText, minute) <- exhaustive_Minute_valid
  (secondText, second') <- exhaustive_Second_valid

  let text = hourText <> minuteText <> secondText
  let hourMinuteSecond = HourMinuteSecond hour minute second'
  pure (text, localTime hourMinuteSecond)

exhaustive_LocalTime_valid_HourMinute :: [(Text, LocalTime)]
exhaustive_LocalTime_valid_HourMinute = do
  (hourText, hour) <- exhaustive_Hour_valid
  (minuteText, minute) <- exhaustive_Minute_valid

  let text = hourText <> minuteText
  let hourMinute = HourMinute hour minute
  pure (text, localTime hourMinute)

exhaustive_LocalTime_valid_MinuteSecond :: [(Text, LocalTime)]
exhaustive_LocalTime_valid_MinuteSecond = do
  (minuteText, minute) <- exhaustive_Minute_valid
  (secondText, second') <- exhaustive_Second_valid

  let text = "-" <> minuteText <> secondText
  let minuteSecond = MinuteSecond minute second'
  pure (text, localTime minuteSecond)

-- Invalid LocalTimes
exhaustive_LocalTime_invalid :: [Text]
exhaustive_LocalTime_invalid =
  concat
    [ exhaustive_LocalTime_invalid_Hour,
      exhaustive_LocalTime_invalid_Minute,
      exhaustive_LocalTime_invalid_Second,
      exhaustive_LocalTime_invalid_HourMinuteSecond,
      exhaustive_LocalTime_invalid_HourMinute,
      exhaustive_LocalTime_invalid_MinuteSecond
    ]

exhaustive_LocalTime_invalid_Hour :: [Text]
exhaustive_LocalTime_invalid_Hour =
  universe_LocalTime_Hour `minus` map fst exhaustive_LocalTime_valid_Hour

exhaustive_LocalTime_invalid_Minute :: [Text]
exhaustive_LocalTime_invalid_Minute =
  universe_LocalTime_Minute `minus` map fst exhaustive_LocalTime_valid_Minute

exhaustive_LocalTime_invalid_Second :: [Text]
exhaustive_LocalTime_invalid_Second =
  universe_LocalTime_Second `minus` map fst exhaustive_LocalTime_valid_Second

exhaustive_LocalTime_invalid_HourMinuteSecond :: [Text]
exhaustive_LocalTime_invalid_HourMinuteSecond =
  universe_LocalTime_HourMinuteSecond
    `minus` map fst exhaustive_LocalTime_valid_HourMinuteSecond

exhaustive_LocalTime_invalid_HourMinute :: [Text]
exhaustive_LocalTime_invalid_HourMinute =
  universe_LocalTime_HourMinute
    `minus` map fst exhaustive_LocalTime_valid_HourMinute

exhaustive_LocalTime_invalid_MinuteSecond :: [Text]
exhaustive_LocalTime_invalid_MinuteSecond =
  universe_LocalTime_MinuteSecond
    `minus` map fst exhaustive_LocalTime_valid_MinuteSecond

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

-------------------------------------
-- Hour, Minute, Second  construction
-------------------------------------
h :: Integer -> Hour
h hour = Hour (finite hour)

m :: Integer -> Minute
m minute = Minute (finite minute)

s :: Integer -> Second
s second' = Second (finite second')

hms :: Integer -> Integer -> Integer -> HourMinuteSecond
hms hour minute second' = HourMinuteSecond (h hour) (m minute) (s second')

hm :: Integer -> Integer -> HourMinute
hm hour minute = HourMinute (h hour) (m minute)

ms :: Integer -> Integer -> MinuteSecond
ms minute second' = MinuteSecond (m minute) (s second')

localTime ::
  (a :| '[Hour, Minute, Second, HourMinuteSecond, HourMinute, MinuteSecond]) =>
  a ->
  LocalTime
localTime = LocalTime . Vary.from

time ::
  (a :| '[Hour, Minute, Second, HourMinuteSecond, HourMinute, MinuteSecond]) =>
  a ->
  Maybe Zone ->
  Time
time lt = Time (localTime lt)
