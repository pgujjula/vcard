-- SPDX-FileCopyrightText: Copyright Preetham Gujjula
-- SPDX-License-Identifier: BSD-3-Clause
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Test.VCard.CaseInsensitive (tests) where

import Data.Dynamic (Dynamic, toDyn)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, testCase, (@?=))
import VCard.CaseInsensitive
  ( CaseInsensitiveLower (..),
    CaseInsensitiveUpper (..),
  )
import VCard.Parse (parse)
import VCard.Serialize (serialize)
import VCard.Symbol.Private (symbolSing)

tests :: TestTree
tests =
  testGroup
    "CaseInsensitive"
    [ testGroup "CaseInsensitiveLower" $
        seq
          test_CaseInsensitiveLower_Type
          [ test_CaseInsensitiveLower_eq,
            test_CaseInsensitiveLower_parse,
            test_CaseInsensitiveLower_serialize
          ],
      testGroup "CaseInsensitiveUpper" $
        seq
          test_CaseInsensitiveUpper_Type
          [ test_CaseInsensitiveUpper_eq,
            test_CaseInsensitiveUpper_parse,
            test_CaseInsensitiveUpper_serialize
          ]
    ]

test_CaseInsensitiveLower_Type :: [Dynamic]
test_CaseInsensitiveLower_Type =
  [ toDyn
      ( CaseInsensitiveLower (symbolSing @"") ::
          CaseInsensitiveLower ""
      ),
    toDyn
      ( CaseInsensitiveLower (symbolSing @"abc") ::
          CaseInsensitiveLower "abc"
      ),
    toDyn
      ( CaseInsensitiveLower (symbolSing @"DEF") ::
          CaseInsensitiveLower "def"
      ),
    toDyn
      ( CaseInsensitiveLower (symbolSing @"Foo Bar") ::
          CaseInsensitiveLower "foo bar"
      )
  ]

test_CaseInsensitiveLower_eq :: TestTree
test_CaseInsensitiveLower_eq =
  testCase "(==)" $ do
    CaseInsensitiveLower (symbolSing @"")
      @?= CaseInsensitiveLower (symbolSing @"")
    CaseInsensitiveLower (symbolSing @"abc")
      @?= CaseInsensitiveLower (symbolSing @"abc")
    assertBool "expected unequal" $
      CaseInsensitiveLower (symbolSing @"abc")
        /= CaseInsensitiveLower (symbolSing @"ABC")

test_CaseInsensitiveLower_parse :: TestTree
test_CaseInsensitiveLower_parse =
  testCase "parse" $ do
    parse @(CaseInsensitiveLower "") ""
      @?= Just (CaseInsensitiveLower (symbolSing @""))
    parse @(CaseInsensitiveLower "foo") "foo"
      @?= Just (CaseInsensitiveLower (symbolSing @"foo"))
    parse @(CaseInsensitiveLower "foo") "Foo"
      @?= Just (CaseInsensitiveLower (symbolSing @"Foo"))
    parse @(CaseInsensitiveLower "foo") "FOO"
      @?= Just (CaseInsensitiveLower (symbolSing @"FOO"))
    --
    parse @(CaseInsensitiveLower "Foo") "foo" @?= Nothing
    parse @(CaseInsensitiveLower "Foo") "Foo" @?= Nothing
    parse @(CaseInsensitiveLower "Foo") "FOO" @?= Nothing
    --
    parse @(CaseInsensitiveLower "foo") "" @?= Nothing
    parse @(CaseInsensitiveLower "foo") "fo" @?= Nothing
    parse @(CaseInsensitiveLower "foo") "fooo" @?= Nothing

test_CaseInsensitiveLower_serialize :: TestTree
test_CaseInsensitiveLower_serialize =
  testCase "serialize" $ do
    serialize (CaseInsensitiveLower (symbolSing @"")) @?= ""
    serialize (CaseInsensitiveLower (symbolSing @"foo")) @?= "foo"
    serialize (CaseInsensitiveLower (symbolSing @"Foo")) @?= "Foo"
    serialize (CaseInsensitiveLower (symbolSing @"FOO")) @?= "FOO"

test_CaseInsensitiveUpper_Type :: [Dynamic]
test_CaseInsensitiveUpper_Type =
  [ toDyn
      ( CaseInsensitiveUpper (symbolSing @"") ::
          CaseInsensitiveUpper ""
      ),
    toDyn
      ( CaseInsensitiveUpper (symbolSing @"abc") ::
          CaseInsensitiveUpper "ABC"
      ),
    toDyn
      ( CaseInsensitiveUpper (symbolSing @"DEF") ::
          CaseInsensitiveUpper "DEF"
      ),
    toDyn
      ( CaseInsensitiveUpper (symbolSing @"Foo Bar") ::
          CaseInsensitiveUpper "FOO BAR"
      )
  ]

test_CaseInsensitiveUpper_eq :: TestTree
test_CaseInsensitiveUpper_eq =
  testCase "(==)" $ do
    CaseInsensitiveUpper (symbolSing @"")
      @?= CaseInsensitiveUpper (symbolSing @"")
    CaseInsensitiveUpper (symbolSing @"abc")
      @?= CaseInsensitiveUpper (symbolSing @"abc")
    assertBool "expected unequal" $
      CaseInsensitiveUpper (symbolSing @"abc")
        /= CaseInsensitiveUpper (symbolSing @"ABC")

test_CaseInsensitiveUpper_parse :: TestTree
test_CaseInsensitiveUpper_parse =
  testCase "parse" $ do
    parse @(CaseInsensitiveUpper "") ""
      @?= Just (CaseInsensitiveUpper (symbolSing @""))
    parse @(CaseInsensitiveUpper "FOO") "foo"
      @?= Just (CaseInsensitiveUpper (symbolSing @"foo"))
    parse @(CaseInsensitiveUpper "FOO") "Foo"
      @?= Just (CaseInsensitiveUpper (symbolSing @"Foo"))
    parse @(CaseInsensitiveUpper "FOO") "FOO"
      @?= Just (CaseInsensitiveUpper (symbolSing @"FOO"))
    --
    parse @(CaseInsensitiveUpper "Foo") "foo" @?= Nothing
    parse @(CaseInsensitiveUpper "Foo") "Foo" @?= Nothing
    parse @(CaseInsensitiveUpper "Foo") "FOO" @?= Nothing
    --
    parse @(CaseInsensitiveUpper "FOO") "" @?= Nothing
    parse @(CaseInsensitiveUpper "FOO") "fo" @?= Nothing
    parse @(CaseInsensitiveUpper "FOO") "fooo" @?= Nothing

test_CaseInsensitiveUpper_serialize :: TestTree
test_CaseInsensitiveUpper_serialize =
  testCase "serialize" $ do
    serialize (CaseInsensitiveUpper (symbolSing @"")) @?= ""
    serialize (CaseInsensitiveUpper (symbolSing @"foo")) @?= "foo"
    serialize (CaseInsensitiveUpper (symbolSing @"Foo")) @?= "Foo"
    serialize (CaseInsensitiveUpper (symbolSing @"FOO")) @?= "FOO"
