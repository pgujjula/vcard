-- SPDX-FileCopyrightText: Copyright Preetham Gujjula
-- SPDX-License-Identifier: BSD-3-Clause
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Test.VCard.Types.Value.Text (tests) where

import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.List.NonEmpty qualified as NE
import Data.Text qualified
import Test.Tasty (TestName, TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))
import VCard.Parse (parse)
import VCard.Serialize (serialize)
import VCard.Types.Value.Text (Text (..))

tests :: TestTree
tests = testGroup "Text" [textTests]

--
-- Text
--
textTests :: TestTree
textTests = testGroup "Text" [validTextTests, invalidTextTests]

validTextTests :: TestTree
validTextTests = testGroup "valid" $
  flip map (NE.toList validTexts) $ \(name, text, value) ->
    testCase name $ do
      parse text @?= Just value
      serialize value @?= text

validTexts :: NonEmpty (TestName, Data.Text.Text, Text)
validTexts =
  ("basic", "Doc", Text "Doc")
    :| [ ("empty", "", Text ""),
         ("backslash", "Doc\\\\Sportello", Text "Doc\\Sportello"),
         ("comma", "Sportello\\,Doc", Text "Sportello,Doc"),
         ("newline", "Doc\\nSportello", Text "Doc\nSportello"),
         ("whitespace_space", "Doc Sportello", Text "Doc Sportello"),
         ("whitespace_tab", "Doc\tSportello", Text "Doc\tSportello"),
         ("non_ascii_1", "Vice Caché", Text "Vice Caché"),
         ("non_ascii_2", "固有瑕疵", Text "固有瑕疵")
       ]

invalidTextTests :: TestTree
invalidTextTests =
  testGroup "invalid" $ flip map invalidTexts $ \(name, text) ->
    testCase name (parse @Text text @?= Nothing)

invalidTexts :: [(TestName, Data.Text.Text)]
invalidTexts =
  [ ("unencoded_backslash", "Doc\\ Sportello"),
    ("unencoded_comma", "Sportello, Doc"),
    ("unencoded_newline", "Doc\nSportello"),
    ("ascii_control_character_null", "Doc\NULSportello"),
    ("ascii_control_character_bell", "Doc\BELSportello"),
    ("crlf_space", "Doc\r\n Sportello")
  ]
