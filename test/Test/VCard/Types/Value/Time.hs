-- SPDX-FileCopyrightText: Copyright Preetham Gujjula
-- SPDX-License-Identifier: BSD-3-Clause
{-# LANGUAGE CPP #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Test.VCard.Types.Value.Time (tests) where

import Control.Monad (forM_, replicateM)
import Data.Finite (finite, finites)
import Data.List.Ordered (minus)
import Data.Proxy (Proxy (..))
import Data.Text (Text)
import Data.Text qualified as Text
import Test.Tasty (TestName, TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))
import TextShow (showt)
import VCard.Parse (HasParser, parse)
import VCard.Serialize (HasSerializer, serialize)
import VCard.Types.Value.Time
  ( Hour (..),
    HourMinuteSecond (..),
    Minute (..),
    Second (..),
    Sign (..),
    Zone (..),
  )

{-# ANN module ("HLint: ignore Use camelCase" :: String) #-}

tests :: TestTree
tests =
  testGroup
    "Time"
    [ test_Hour,
      test_Minute,
      test_Second,
      test_HourMinuteSecond,
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

units_Hour_valid :: [(Text, Hour)]
units_Hour_valid =
  [ ("00", Hour 00),
    ("02", Hour 02),
    ("15", Hour 15),
    ("23", Hour 23)
  ]

units_Hour_invalidSemantics :: [Text]
units_Hour_invalidSemantics =
  ["24", "25", "26", "50", "99"]

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

units_Minute_valid :: [(Text, Minute)]
units_Minute_valid =
  [ ("00", Minute 00),
    ("08", Minute 08),
    ("45", Minute 45),
    ("59", Minute 59)
  ]

units_Minute_invalidSemantics :: [Text]
units_Minute_invalidSemantics =
  ["60", "61", "62", "63", "75", "99"]

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

units_Second_valid :: [(Text, Second)]
units_Second_valid =
  [ ("00", Second 00),
    ("08", Second 08),
    ("45", Second 45),
    ("59", Second 59),
    ("60", Second 60)
  ]

units_Second_invalidSemantics :: [Text]
units_Second_invalidSemantics =
  ["61", "62", "63", "75", "99"]

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
  testSerialize "unit" units_Sign_valid

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
  testSerialize "unit" units_Zone_valid

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
s second = Second (finite second)

hms :: Integer -> Integer -> Integer -> HourMinuteSecond
hms hour minute second = HourMinuteSecond (h hour) (m minute) (s second)
