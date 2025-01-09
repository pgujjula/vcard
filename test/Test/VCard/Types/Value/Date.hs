-- SPDX-FileCopyrightText: Copyright Preetham Gujjula
-- SPDX-License-Identifier: BSD-3-Clause
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Test.VCard.Types.Value.Date (tests) where

import Control.Monad (forM_)
import Data.Finite (finite)
import Data.Text (Text)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))
import TextShow (showt)
import VCard.Parse (parse)
import VCard.Serialize (serialize)
import VCard.Types.Value.Date (Day (..))

tests :: TestTree
tests = testGroup "Date" [dayTests]

--
-- Day
--
dayTests :: TestTree
dayTests = testGroup "Day" [validDayTests, invalidDayTests]

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
