-- SPDX-FileCopyrightText: Copyright Preetham Gujjula
-- SPDX-License-Identifier: BSD-3-Clause
{-# LANGUAGE OverloadedStrings #-}

module Test.VCard.Types.Param.ParamValue (tests) where

import Control.Monad (forM_)
import Data.Maybe (isJust, isNothing)
import Data.Proxy (Proxy (..))
import Data.Text (Text)
import Test.Tasty (TestName, TestTree, testGroup)
import Test.Tasty.HUnit (Assertion, assertBool, assertFailure, testCase, (@?=))
import VCard.Parse (HasParser, parse)
import VCard.Serialize (HasSerializer, serialize)
import VCard.Symbol.Private
  ( SSymbol,
    symbolSing,
    testSSymbolEquality,
  )
import VCard.Types.Param.ParamValue
  ( ParamValue,
    ParamValueSymbol,
    SParamValue (..),
    SomeParamValue (..),
    paramValueVal,
    sUnquoteSParamValue,
    someParamValueVal,
    testParamValueSymbol,
    unParamValue,
    unquoteParamValue,
  )

{-# ANN module ("HLint: ignore Use camelCase" :: String) #-}

tests :: TestTree
tests =
  testGroup
    "ParamValue"
    [ test_testParamValueSymbol,
      test_paramValueVal,
      test_someParamValueVal,
      test_parse,
      test_serialize,
      test_unquoteParamValue,
      test_sUnquoteParamValue
    ]

test_testParamValueSymbol :: TestTree
test_testParamValueSymbol =
  testCase "testParamValueSymbol" $ do
    let assertParamValueSymbol :: SSymbol s -> Assertion
        assertParamValueSymbol ss =
          assertBool
            ("Expected " <> show ss <> " to have a ParamValueSymbol constraint")
            (isJust (testParamValueSymbol ss))

        assertNoParamValueSymbol :: SSymbol s -> Assertion
        assertNoParamValueSymbol ss =
          assertBool
            ( "Expected "
                <> show ss
                <> " not to have a ParamValueSymbol constraint"
            )
            (isNothing (testParamValueSymbol ss))

    --
    -- QSafe symbols
    --
    assertParamValueSymbol (symbolSing @"\"\"")
    assertParamValueSymbol (symbolSing @"\"a\"")
    assertParamValueSymbol (symbolSing @"\"Foo\"")

    assertParamValueSymbol (symbolSing @"\" \t\"")
    assertParamValueSymbol (symbolSing @"\":;\"")
    assertParamValueSymbol (symbolSing @"\"!#\'$0123abcABC|}~\"")
    assertParamValueSymbol (symbolSing @"\"€é‰\"")

    assertNoParamValueSymbol (symbolSing @"\"foo\nbar\"")
    assertNoParamValueSymbol (symbolSing @"\"foo\"bar\"")

    assertNoParamValueSymbol (symbolSing @"\"foobar")
    assertNoParamValueSymbol (symbolSing @"foobar\"")

    --
    -- Safe symbols
    --
    assertParamValueSymbol (symbolSing @"")
    assertParamValueSymbol (symbolSing @"a")
    assertParamValueSymbol (symbolSing @"Foo")

    assertParamValueSymbol (symbolSing @" \t")
    assertParamValueSymbol (symbolSing @"!#$\'0123abcABC|}~")
    assertParamValueSymbol (symbolSing @"€é‰")

    assertNoParamValueSymbol (symbolSing @"foo;bar")
    assertNoParamValueSymbol (symbolSing @"foo:bar")
    assertNoParamValueSymbol (symbolSing @"foo,bar")
    assertNoParamValueSymbol (symbolSing @"foo\nbar")
    assertNoParamValueSymbol (symbolSing @"foo\"bar")

test_paramValueVal :: TestTree
test_paramValueVal =
  testCase "paramValueVal" $ do
    unParamValue (paramValueVal (SParamValue (symbolSing @""))) @?= ""
    unParamValue (paramValueVal (SParamValue (symbolSing @"abc"))) @?= "abc"
    unParamValue (paramValueVal (SParamValue (symbolSing @"\"Foo Bar;\"")))
      @?= "\"Foo Bar;\""

test_someParamValueVal :: TestTree
test_someParamValueVal =
  testCase "someParamValueVal" $ do
    let testWith :: (ParamValueSymbol s) => SSymbol s -> Assertion
        testWith ss =
          case someParamValueVal (paramValueVal (SParamValue ss)) of
            SomeParamValue (SParamValue ss') ->
              assertEqualSSymbol ss' ss
    testWith (symbolSing @"")
    testWith (symbolSing @"a")
    testWith (symbolSing @"foo")
    testWith (symbolSing @"\"\"")
    testWith (symbolSing @"\"a\"")
    testWith (symbolSing @"\"foo;\"")

test_parse :: TestTree
test_parse =
  testGroup
    "parse"
    [ test_parse_ParamValue,
      test_parse_SParamValue,
      test_parse_SomeParamValue
    ]

test_parse_ParamValue :: TestTree
test_parse_ParamValue =
  testGroup
    "ParamValue"
    [ testParseValid cases_ParamValue_valid,
      testParseInvalid (Proxy @ParamValue) cases_ParamValue_invalid
    ]

test_parse_SParamValue :: TestTree
test_parse_SParamValue =
  testCase "SParamValue" $ do
    -- QSafe symbols
    assertEqualSParamValue
      (parse @(SParamValue "\"\"") "\"\"")
      (Just (SParamValue (symbolSing @"\"\"")))
    assertEqualSParamValue
      (parse @(SParamValue "\"foo\"") "\"foo\"")
      (Just (SParamValue (symbolSing @"\"foo\"")))
    assertEqualSParamValue
      (parse @(SParamValue "\"foo;\"") "\"foo;\"")
      (Just (SParamValue (symbolSing @"\"foo;\"")))
    -- Safe symbols
    assertEqualSParamValue
      (parse @(SParamValue "") "")
      (Just (SParamValue (symbolSing @"")))
    assertEqualSParamValue
      (parse @(SParamValue "foo") "foo")
      (Just (SParamValue (symbolSing @"foo")))

    -- Cannot parse quoted symbol as unquoted, or vice versa
    assertEqualSParamValue (parse @(SParamValue "foo") "\"foo\"") Nothing
    assertEqualSParamValue (parse @(SParamValue "\"foo\"") "foo") Nothing

    -- Cannot parse incorrectly quoted symbol
    assertEqualSParamValue (parse @(SParamValue "foo") "foo\"") Nothing
    assertEqualSParamValue (parse @(SParamValue "\"foo\"") "foo\"") Nothing
    assertEqualSParamValue (parse @(SParamValue "foo") "\"foo") Nothing
    assertEqualSParamValue (parse @(SParamValue "\"foo\"") "\"foo") Nothing

    -- Cannot parse symbol without ParamValueSymbol constraint
    assertEqualSParamValue (parse @(SParamValue "foo;") "foo;") Nothing
    assertEqualSParamValue (parse @(SParamValue "foo:") "foo:") Nothing
    assertEqualSParamValue (parse @(SParamValue "foo,bar") "foo,bar") Nothing

test_parse_SomeParamValue :: TestTree
test_parse_SomeParamValue =
  testGroup
    "SomeParamValue"
    [ testParseValid cases_SomeParamValue_valid,
      testParseInvalid (Proxy @SomeParamValue) cases_SomeParamValue_invalid
    ]

test_serialize :: TestTree
test_serialize =
  testGroup
    "serialize"
    [ test_serialize_ParamValue,
      test_serialize_SParamValue,
      test_serialize_SomeParamValue
    ]

test_serialize_ParamValue :: TestTree
test_serialize_ParamValue = testSerialize "ParamValue" cases_ParamValue_valid

test_serialize_SParamValue :: TestTree
test_serialize_SParamValue =
  testCase "SParamValue" $ do
    serialize (SParamValue (symbolSing @"")) @?= ""
    serialize (SParamValue (symbolSing @"foo")) @?= "foo"
    serialize (SParamValue (symbolSing @"\"foo\"")) @?= "\"foo\""
    serialize (SParamValue (symbolSing @"\"foo;\"")) @?= "\"foo;\""

test_serialize_SomeParamValue :: TestTree
test_serialize_SomeParamValue =
  testSerialize "SomeParamValue" cases_SomeParamValue_valid

cases_ParamValue_valid :: [(Text, ParamValue)]
cases_ParamValue_valid =
  [ --
    -- QSafe
    --
    ( "\"\"",
      paramValueVal (SParamValue (symbolSing @"\"\""))
    ),
    ( "\"a\"",
      paramValueVal (SParamValue (symbolSing @"\"a\""))
    ),
    ( "\"Foo\"",
      paramValueVal (SParamValue (symbolSing @"\"Foo\""))
    ),
    ( "\" \t\"",
      paramValueVal (SParamValue (symbolSing @"\" \t\""))
    ),
    ( "\":;\"",
      paramValueVal (SParamValue (symbolSing @"\":;\""))
    ),
    ( "\"!#\'$0123abcABC|}~\"",
      paramValueVal (SParamValue (symbolSing @"\"!#\'$0123abcABC|}~\""))
    ),
    ( "\"€é‰\"",
      paramValueVal (SParamValue (symbolSing @"\"€é‰\""))
    ),
    --
    -- Safe
    --
    ( "",
      paramValueVal (SParamValue (symbolSing @""))
    ),
    ( "a",
      paramValueVal (SParamValue (symbolSing @"a"))
    ),
    ( "Foo",
      paramValueVal (SParamValue (symbolSing @"Foo"))
    ),
    ( " \t",
      paramValueVal (SParamValue (symbolSing @" \t"))
    ),
    ( "!#$\'0123abcABC|}~",
      paramValueVal (SParamValue (symbolSing @"!#$\'0123abcABC|}~"))
    ),
    ( "€é‰",
      paramValueVal (SParamValue (symbolSing @"€é‰"))
    )
  ]

cases_SomeParamValue_valid :: [(Text, SomeParamValue)]
cases_SomeParamValue_valid =
  [ --
    -- QSafe
    --
    ( "\"\"",
      SomeParamValue (SParamValue (symbolSing @"\"\""))
    ),
    ( "\"a\"",
      SomeParamValue (SParamValue (symbolSing @"\"a\""))
    ),
    ( "\"Foo\"",
      SomeParamValue (SParamValue (symbolSing @"\"Foo\""))
    ),
    ( "\" \t\"",
      SomeParamValue (SParamValue (symbolSing @"\" \t\""))
    ),
    ( "\":;\"",
      SomeParamValue (SParamValue (symbolSing @"\":;\""))
    ),
    ( "\"!#\'$0123abcABC|}~\"",
      SomeParamValue (SParamValue (symbolSing @"\"!#\'$0123abcABC|}~\""))
    ),
    ( "\"€é‰\"",
      SomeParamValue (SParamValue (symbolSing @"\"€é‰\""))
    ),
    --
    -- Safe
    --
    ( "",
      SomeParamValue (SParamValue (symbolSing @""))
    ),
    ( "a",
      SomeParamValue (SParamValue (symbolSing @"a"))
    ),
    ( "Foo",
      SomeParamValue (SParamValue (symbolSing @"Foo"))
    ),
    ( " \t",
      SomeParamValue (SParamValue (symbolSing @" \t"))
    ),
    ( "!#$\'0123abcABC|}~",
      SomeParamValue (SParamValue (symbolSing @"!#$\'0123abcABC|}~"))
    ),
    ( "€é‰",
      SomeParamValue (SParamValue (symbolSing @"€é‰"))
    )
  ]

cases_ParamValue_invalid :: [Text]
cases_ParamValue_invalid =
  [ "\"foo\nbar\"",
    "\"foo\"bar\"",
    "\"foobar",
    "foobar\"",
    "foo;bar",
    "foo:bar",
    "foo,bar",
    "foo\nbar",
    "foo\"bar"
  ]

cases_SomeParamValue_invalid :: [Text]
cases_SomeParamValue_invalid = cases_ParamValue_invalid

--
-- Unquoting
--
test_unquoteParamValue :: TestTree
test_unquoteParamValue =
  testCase "unquoteParamValue" $ do
    unquoteParamValue (paramValueVal (SParamValue (symbolSing @""))) @?= ""
    unquoteParamValue (paramValueVal (SParamValue (symbolSing @"foo")))
      @?= "foo"
    unquoteParamValue (paramValueVal (SParamValue (symbolSing @"\"foo\"")))
      @?= "foo"
    unquoteParamValue (paramValueVal (SParamValue (symbolSing @"\"foo;\"")))
      @?= "foo;"

test_sUnquoteParamValue :: TestTree
test_sUnquoteParamValue =
  testCase "sUnquoteParmaValue" $ do
    assertEqualSSymbol
      (sUnquoteSParamValue (SParamValue (symbolSing @"")))
      (symbolSing @"")
    assertEqualSSymbol
      (sUnquoteSParamValue (SParamValue (symbolSing @"foo")))
      (symbolSing @"foo")
    assertEqualSSymbol
      (sUnquoteSParamValue (SParamValue (symbolSing @"\"foo\"")))
      (symbolSing @"foo")

-- Utilities
assertEqualSSymbol :: SSymbol a -> SSymbol b -> Assertion
assertEqualSSymbol sa sb =
  assertBool "expected equal SSymbols" $
    isJust (testSSymbolEquality sa sb)

assertEqualSParamValue ::
  Maybe (SParamValue a) -> Maybe (SParamValue b) -> Assertion
assertEqualSParamValue Nothing Nothing = pure ()
assertEqualSParamValue (Just _) Nothing =
  assertFailure "expected equal SParamValues"
assertEqualSParamValue Nothing (Just _) =
  assertFailure "expected equal SParamValues"
assertEqualSParamValue (Just (SParamValue sa)) (Just (SParamValue sb)) =
  assertEqualSSymbol sa sb

testParseValid :: (Show a, Eq a, HasParser a) => [(Text, a)] -> TestTree
testParseValid cases =
  testCase "valid" $
    forM_ cases $ \(text, value) ->
      parse text @?= Just value

testParseInvalid ::
  forall a.
  (Show a, Eq a, HasParser a) =>
  Proxy a ->
  [Text] ->
  TestTree
testParseInvalid _ cases =
  testCase "invalid" $
    forM_ cases $ \text ->
      parse text @?= (Nothing :: Maybe a)

testSerialize :: (HasSerializer a) => TestName -> [(Text, a)] -> TestTree
testSerialize name cases =
  testCase name $
    forM_ cases $ \(text, value) ->
      serialize value @?= text
