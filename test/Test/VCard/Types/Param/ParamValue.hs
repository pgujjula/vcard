-- SPDX-FileCopyrightText: Copyright Preetham Gujjula
-- SPDX-License-Identifier: BSD-3-Clause
{-# LANGUAGE OverloadedStrings #-}

module Test.VCard.Types.Param.ParamValue (tests) where

import Data.Maybe (isJust, isNothing)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (Assertion, assertBool, testCase, (@?=))
import VCard.Symbol.Private (SSymbol, symbolSing, testSSymbolEquality)
import VCard.Types.Param.ParamValue
  ( ParamValueSymbol,
    SParamValue (..),
    SomeParamValue (..),
    paramValueVal,
    someParamValueVal,
    testParamValueSymbol,
    unParamValue,
  )

tests :: TestTree
tests =
  testGroup
    "ParamValue"
    [ test_testParamValueSymbol,
      test_paramValueVal,
      test_someParamValueVal
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

-- Utilities
assertEqualSSymbol :: SSymbol a -> SSymbol b -> Assertion
assertEqualSSymbol sa sb =
  assertBool "expected equal SSymbols" $
    isJust (testSSymbolEquality sa sb)
