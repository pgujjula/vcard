-- SPDX-FileCopyrightText: Copyright Preetham Gujjula
-- SPDX-License-Identifier: BSD-3-Clause
{-# LANGUAGE OverloadedStrings #-}

module Test.VCard.Types.Textual.Private.XName (tests) where

import Data.Maybe (isJust, isNothing)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (Assertion, assertBool, testCase, (@?=))
import Test.VCard.Types.Textual.Private.XName.TypeTests
  ( test_XNameLowerSymbol,
    test_XNameSymbol,
    test_XNameUpperSymbol,
  )
import VCard.Parse (parse)
import VCard.Serialize (serialize)
import VCard.Symbol.Private (symbolSing)
import VCard.Types.Textual.Private.XName
  ( SXName (..),
    SomeXName (..),
    XName (..),
    someXNameVal,
    testXNameLowerSymbol,
    testXNameSymbol,
    testXNameUpperSymbol,
    xNameVal,
  )

tests :: TestTree
tests =
  testGroup
    "XName"
    [ test_XNameSymbol,
      test_testXNameSymbol,
      test_XName,
      test_SXName,
      test_SomeXName,
      test_xNameVal,
      test_someXNameVal,
      test_XNameLowerSymbol,
      test_testXNameLowerSymbol,
      test_XNameUpperSymbol,
      test_testXNameUpperSymbol
    ]

assertIsJust :: Maybe a -> Assertion
assertIsJust x = assertBool "expected Just" (isJust x)

assertIsNothing :: Maybe a -> Assertion
assertIsNothing x = assertBool "expected Nothing" (isNothing x)

test_testXNameSymbol :: TestTree
test_testXNameSymbol =
  testCase "testXNameSymbol" $ do
    assertIsJust (testXNameSymbol (symbolSing @"x-a"))
    assertIsJust (testXNameSymbol (symbolSing @"X-a"))
    assertIsJust (testXNameSymbol (symbolSing @"x-A"))
    assertIsJust (testXNameSymbol (symbolSing @"X-A"))
    assertIsJust (testXNameSymbol (symbolSing @"x-foo"))
    assertIsJust (testXNameSymbol (symbolSing @"X-foo"))
    assertIsJust (testXNameSymbol (symbolSing @"x-Foo"))
    assertIsJust (testXNameSymbol (symbolSing @"X-Foo"))
    assertIsJust (testXNameSymbol (symbolSing @"x-FOO"))
    assertIsJust (testXNameSymbol (symbolSing @"X-FOO"))
    assertIsJust (testXNameSymbol (symbolSing @"x-123"))
    assertIsJust (testXNameSymbol (symbolSing @"X-123"))
    assertIsJust (testXNameSymbol (symbolSing @"x-a1"))
    assertIsJust (testXNameSymbol (symbolSing @"X-a1"))
    assertIsJust (testXNameSymbol (symbolSing @"x-A1"))
    assertIsJust (testXNameSymbol (symbolSing @"X-A1"))
    assertIsJust (testXNameSymbol (symbolSing @"x--"))
    assertIsJust (testXNameSymbol (symbolSing @"x-a--a"))
    --
    assertIsNothing (testXNameSymbol (symbolSing @""))
    assertIsNothing (testXNameSymbol (symbolSing @"a"))
    assertIsNothing (testXNameSymbol (symbolSing @"A"))
    assertIsNothing (testXNameSymbol (symbolSing @"foo"))
    assertIsNothing (testXNameSymbol (symbolSing @"Foo"))
    assertIsNothing (testXNameSymbol (symbolSing @"FOO"))
    assertIsNothing (testXNameSymbol (symbolSing @"x"))
    assertIsNothing (testXNameSymbol (symbolSing @"X"))
    assertIsNothing (testXNameSymbol (symbolSing @"x-"))
    assertIsNothing (testXNameSymbol (symbolSing @"X-"))
    assertIsNothing (testXNameSymbol (symbolSing @" "))
    assertIsNothing (testXNameSymbol (symbolSing @" x-a"))
    assertIsNothing (testXNameSymbol (symbolSing @"x-a "))
    assertIsNothing (testXNameSymbol (symbolSing @"x- a"))
    assertIsNothing (testXNameSymbol (symbolSing @"x-é"))

test_XName :: TestTree
test_XName =
  testGroup
    "XName"
    [ test_XName_parse,
      test_XName_serialize
    ]

-- TODO: Factor out the common test cases between parse and serialize
test_XName_parse :: TestTree
test_XName_parse =
  testCase "parse" $ do
    parse @XName "x-a" @?= Just (xNameVal (SXName (symbolSing @"x-a")))
    parse @XName "x-foo" @?= Just (xNameVal (SXName (symbolSing @"x-foo")))
    parse @XName "X-Foo" @?= Just (xNameVal (SXName (symbolSing @"X-Foo")))
    parse @XName "x--FOO-bar13-14--"
      @?= Just (xNameVal (SXName (symbolSing @"x--FOO-bar13-14--")))

    parse @XName "x-" @?= Nothing
    parse @XName "X-" @?= Nothing
    parse @XName "X-abc!" @?= Nothing
    parse @XName "abc" @?= Nothing
    parse @XName "xabc" @?= Nothing

test_XName_serialize :: TestTree
test_XName_serialize =
  testCase "serialize" $ do
    serialize
      (xNameVal (SXName (symbolSing @"x-a")))
      @?= "x-a"
    serialize (xNameVal (SXName (symbolSing @"x-foo")))
      @?= "x-foo"
    serialize (xNameVal (SXName (symbolSing @"X-Foo")))
      @?= "X-Foo"
    serialize (xNameVal (SXName (symbolSing @"x--FOO-bar13-14--")))
      @?= "x--FOO-bar13-14--"

test_SXName :: TestTree
test_SXName =
  testGroup
    "SXName"
    [ test_SXName_parse,
      test_SXName_serialize
    ]

test_SXName_parse :: TestTree
test_SXName_parse =
  testCase "parse" $ do
    parse @(SXName "x-a") "x-a" @?= Just (SXName (symbolSing @"x-a"))
    parse @(SXName "x-foo") "x-foo" @?= Just (SXName (symbolSing @"x-foo"))
    parse @(SXName "X-Foo") "X-Foo" @?= Just (SXName (symbolSing @"X-Foo"))
    parse @(SXName "x--FOO-bar13-14--") "x--FOO-bar13-14--"
      @?= Just (SXName (symbolSing @"x--FOO-bar13-14--"))

    parse @(SXName "x-") "x-" @?= Nothing
    parse @(SXName "X-") "X-" @?= Nothing
    parse @(SXName "X-abc!") "X-abc!" @?= Nothing
    parse @(SXName "abc") "abc" @?= Nothing
    parse @(SXName "xabc") "xabc" @?= Nothing

    parse @(SXName "x-a") "x-A" @?= Nothing
    parse @(SXName "x-a") "X-a" @?= Nothing
    parse @(SXName "X-A") "x-A" @?= Nothing
    parse @(SXName "X-A") "X-a" @?= Nothing

test_SXName_serialize :: TestTree
test_SXName_serialize =
  testCase "serialize" $ do
    serialize (SXName (symbolSing @"x-a"))
      @?= "x-a"
    serialize (SXName (symbolSing @"x-foo"))
      @?= "x-foo"
    serialize (SXName (symbolSing @"X-Foo"))
      @?= "X-Foo"
    serialize (SXName (symbolSing @"x--FOO-bar13-14--"))
      @?= "x--FOO-bar13-14--"

test_SomeXName :: TestTree
test_SomeXName =
  testGroup
    "SomeXName"
    [ test_SomeXName_parse,
      test_SomeXName_serialize
    ]

test_SomeXName_parse :: TestTree
test_SomeXName_parse =
  testCase "parse" $ do
    parse @SomeXName "x-a" @?= Just (SomeXName (SXName (symbolSing @"x-a")))
    parse @SomeXName "x-foo" @?= Just (SomeXName (SXName (symbolSing @"x-foo")))
    parse @SomeXName "X-Foo" @?= Just (SomeXName (SXName (symbolSing @"X-Foo")))
    parse @SomeXName "x--FOO-bar13-14--"
      @?= Just (SomeXName (SXName (symbolSing @"x--FOO-bar13-14--")))

    parse @SomeXName "x-" @?= Nothing
    parse @SomeXName "X-" @?= Nothing
    parse @SomeXName "X-abc!" @?= Nothing
    parse @SomeXName "abc" @?= Nothing
    parse @SomeXName "xabc" @?= Nothing

test_SomeXName_serialize :: TestTree
test_SomeXName_serialize =
  testCase "serialize" $ do
    serialize
      (SomeXName (SXName (symbolSing @"x-a")))
      @?= "x-a"
    serialize (SomeXName (SXName (symbolSing @"x-foo")))
      @?= "x-foo"
    serialize (SomeXName (SXName (symbolSing @"X-Foo")))
      @?= "X-Foo"
    serialize (SomeXName (SXName (symbolSing @"x--FOO-bar13-14--")))
      @?= "x--FOO-bar13-14--"

test_xNameVal :: TestTree
test_xNameVal =
  testCase "xNameVal" $ do
    unXName (xNameVal (SXName (symbolSing @"x-a"))) @?= "x-a"
    unXName (xNameVal (SXName (symbolSing @"X-Foo"))) @?= "X-Foo"
    unXName (xNameVal (SXName (symbolSing @"x--FOO-bar13-14--")))
      @?= "x--FOO-bar13-14--"

test_someXNameVal :: TestTree
test_someXNameVal =
  testCase "someXNameVal" $ do
    someXNameVal (xNameVal (SXName (symbolSing @"x-a")))
      @?= SomeXName (SXName (symbolSing @"x-a"))
    someXNameVal (xNameVal (SXName (symbolSing @"x-Foo")))
      @?= SomeXName (SXName (symbolSing @"x-Foo"))
    someXNameVal (xNameVal (SXName (symbolSing @"x--FOO-bar13-14--")))
      @?= SomeXName (SXName (symbolSing @"x--FOO-bar13-14--"))

test_testXNameLowerSymbol :: TestTree
test_testXNameLowerSymbol =
  testCase "testXNameLowerSymbol" $ do
    assertIsJust (testXNameLowerSymbol (symbolSing @"x-a"))
    assertIsJust (testXNameLowerSymbol (symbolSing @"x-foo"))
    assertIsJust (testXNameLowerSymbol (symbolSing @"x-123"))
    assertIsJust (testXNameLowerSymbol (symbolSing @"x-a1"))
    assertIsJust (testXNameLowerSymbol (symbolSing @"x--"))
    assertIsJust (testXNameLowerSymbol (symbolSing @"x-a--a"))
    --
    assertIsNothing (testXNameLowerSymbol (symbolSing @"x-Foo"))
    assertIsNothing (testXNameLowerSymbol (symbolSing @"X-Foo"))
    assertIsNothing (testXNameLowerSymbol (symbolSing @"x-FOO"))
    assertIsNothing (testXNameLowerSymbol (symbolSing @"X-FOO"))
    assertIsNothing (testXNameLowerSymbol (symbolSing @"x-A"))
    assertIsNothing (testXNameLowerSymbol (symbolSing @"X-a"))
    assertIsNothing (testXNameLowerSymbol (symbolSing @"X-A"))
    assertIsNothing (testXNameLowerSymbol (symbolSing @"X-foo"))
    assertIsNothing (testXNameLowerSymbol (symbolSing @"X-123"))
    assertIsNothing (testXNameLowerSymbol (symbolSing @"X-a1"))
    assertIsNothing (testXNameLowerSymbol (symbolSing @"x-A1"))
    assertIsNothing (testXNameLowerSymbol (symbolSing @"X-A1"))
    --
    assertIsNothing (testXNameLowerSymbol (symbolSing @""))
    assertIsNothing (testXNameLowerSymbol (symbolSing @"a"))
    assertIsNothing (testXNameLowerSymbol (symbolSing @"A"))
    assertIsNothing (testXNameLowerSymbol (symbolSing @"foo"))
    assertIsNothing (testXNameLowerSymbol (symbolSing @"Foo"))
    assertIsNothing (testXNameLowerSymbol (symbolSing @"FOO"))
    assertIsNothing (testXNameLowerSymbol (symbolSing @"x"))
    assertIsNothing (testXNameLowerSymbol (symbolSing @"X"))
    assertIsNothing (testXNameLowerSymbol (symbolSing @"x-"))
    assertIsNothing (testXNameLowerSymbol (symbolSing @"X-"))
    assertIsNothing (testXNameLowerSymbol (symbolSing @" "))
    assertIsNothing (testXNameLowerSymbol (symbolSing @" x-a"))
    assertIsNothing (testXNameLowerSymbol (symbolSing @"x-a "))
    assertIsNothing (testXNameLowerSymbol (symbolSing @"x- a"))
    assertIsNothing (testXNameLowerSymbol (symbolSing @"x-é"))

test_testXNameUpperSymbol :: TestTree
test_testXNameUpperSymbol =
  testCase "testXNameUpperSymbol" $ do
    assertIsJust (testXNameUpperSymbol (symbolSing @"X-A"))
    assertIsJust (testXNameUpperSymbol (symbolSing @"X-FOO"))
    assertIsJust (testXNameUpperSymbol (symbolSing @"X-123"))
    assertIsJust (testXNameUpperSymbol (symbolSing @"X-A1"))
    assertIsJust (testXNameUpperSymbol (symbolSing @"X--"))
    assertIsJust (testXNameUpperSymbol (symbolSing @"X-A--A"))
    --
    assertIsNothing (testXNameUpperSymbol (symbolSing @"x-FOO"))
    assertIsNothing (testXNameUpperSymbol (symbolSing @"x-foo"))
    assertIsNothing (testXNameUpperSymbol (symbolSing @"X-foo"))
    assertIsNothing (testXNameUpperSymbol (symbolSing @"x-Foo"))
    assertIsNothing (testXNameUpperSymbol (symbolSing @"X-Foo"))
    assertIsNothing (testXNameUpperSymbol (symbolSing @"x-a"))
    assertIsNothing (testXNameUpperSymbol (symbolSing @"X-a"))
    assertIsNothing (testXNameUpperSymbol (symbolSing @"x-A"))
    assertIsNothing (testXNameUpperSymbol (symbolSing @"x-123"))
    assertIsNothing (testXNameUpperSymbol (symbolSing @"x-a1"))
    assertIsNothing (testXNameUpperSymbol (symbolSing @"X-a1"))
    assertIsNothing (testXNameUpperSymbol (symbolSing @"x-A1"))
    --
    assertIsNothing (testXNameUpperSymbol (symbolSing @""))
    assertIsNothing (testXNameUpperSymbol (symbolSing @"a"))
    assertIsNothing (testXNameUpperSymbol (symbolSing @"A"))
    assertIsNothing (testXNameUpperSymbol (symbolSing @"foo"))
    assertIsNothing (testXNameUpperSymbol (symbolSing @"Foo"))
    assertIsNothing (testXNameUpperSymbol (symbolSing @"FOO"))
    assertIsNothing (testXNameUpperSymbol (symbolSing @"x"))
    assertIsNothing (testXNameUpperSymbol (symbolSing @"X"))
    assertIsNothing (testXNameUpperSymbol (symbolSing @"x-"))
    assertIsNothing (testXNameUpperSymbol (symbolSing @"X-"))
    assertIsNothing (testXNameUpperSymbol (symbolSing @" "))
    assertIsNothing (testXNameUpperSymbol (symbolSing @" X-A"))
    assertIsNothing (testXNameUpperSymbol (symbolSing @"X-A "))
    assertIsNothing (testXNameUpperSymbol (symbolSing @"X- A"))
    assertIsNothing (testXNameUpperSymbol (symbolSing @"X-É"))
