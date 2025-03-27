-- SPDX-FileCopyrightText: Copyright Preetham Gujjula
-- SPDX-License-Identifier: BSD-3-Clause
{-# LANGUAGE OverloadedStrings #-}

module Test.VCard.Types.Param.AltID (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (Assertion, assertBool, testCase, (@?=))
import VCard.Parse (parse)
import VCard.Serialize (serialize)
import VCard.Symbol.Private (symbolSing)
import VCard.Types.Param.AltID
  ( AltID,
    SAltID (..),
    SomeAltID (..),
    altIDVal,
    someAltIDVal,
  )
import VCard.Types.Param.Generic (Param (..))
import VCard.Types.Param.ParamValue (SParamValue (..), paramValueVal)
import VCard.Types.Textual (CaseInsensitiveUpper (..))

tests :: TestTree
tests =
  testGroup
    "AltID"
    [ test_altIDVal,
      test_someAltIDVal,
      test_AltID,
      test_SAltID,
      test_SomeAltID
    ]

test_altIDVal :: TestTree
test_altIDVal =
  testCase "altIDVal" $
    let param =
          Param
            { paramName = CaseInsensitiveUpper (symbolSing @"ALTID"),
              paramValue = SParamValue (symbolSing @"foo")
            }
        saltid = SAltID param
        altID =
          Param
            { paramName = CaseInsensitiveUpper (symbolSing @"ALTID"),
              paramValue = paramValueVal (SParamValue (symbolSing @"foo"))
            }
     in altIDVal saltid @?= altID

test_someAltIDVal :: TestTree
test_someAltIDVal =
  testCase "someAltIDVal" $ do
    let altID =
          Param
            { paramName = CaseInsensitiveUpper (symbolSing @"ALTID"),
              paramValue = paramValueVal (SParamValue (symbolSing @"foo"))
            }
        someAltID =
          SomeAltID . SAltID $
            Param
              { paramName = CaseInsensitiveUpper (symbolSing @"ALTID"),
                paramValue = SParamValue (symbolSing @"foo")
              }
     in someAltIDVal altID @?= someAltID

test_AltID :: TestTree
test_AltID =
  testGroup
    "AltID"
    [ test_AltID_parse,
      test_AltID_serialize
    ]

test_AltID_parse :: TestTree
test_AltID_parse =
  testCase "parse" $ do
    parse @AltID "ALTID="
      @?= Just
        ( Param
            { paramName = CaseInsensitiveUpper (symbolSing @"ALTID"),
              paramValue = paramValueVal (SParamValue (symbolSing @""))
            }
        )

    parse @AltID "ALTID=foo"
      @?= Just
        ( Param
            { paramName = CaseInsensitiveUpper (symbolSing @"ALTID"),
              paramValue = paramValueVal (SParamValue (symbolSing @"foo"))
            }
        )

    parse @AltID "ALTID=\"foo\""
      @?= Just
        ( Param
            { paramName = CaseInsensitiveUpper (symbolSing @"ALTID"),
              paramValue = paramValueVal (SParamValue (symbolSing @"\"foo\""))
            }
        )

    parse @AltID "ALTID=foo;" @?= Nothing
    parse @AltID "ALTID=foo,bar" @?= Nothing

test_AltID_serialize :: TestTree
test_AltID_serialize =
  testCase "serialize" $ do
    serialize @AltID
      ( Param
          { paramName = CaseInsensitiveUpper (symbolSing @"ALTID"),
            paramValue = paramValueVal (SParamValue (symbolSing @""))
          }
      )
      @?= "ALTID="

    serialize
      ( Param
          { paramName = CaseInsensitiveUpper (symbolSing @"ALTID"),
            paramValue = paramValueVal (SParamValue (symbolSing @"foo"))
          }
      )
      @?= "ALTID=foo"

    serialize
      ( Param
          { paramName = CaseInsensitiveUpper (symbolSing @"ALTID"),
            paramValue = paramValueVal (SParamValue (symbolSing @"\"foo\""))
          }
      )
      @?= "ALTID=\"foo\""

test_SAltID :: TestTree
test_SAltID =
  testGroup
    "SAltID"
    [ test_SAltID_Eq,
      test_SAltID_parse,
      test_SAltID_serialize
    ]

test_SAltID_Eq :: TestTree
test_SAltID_Eq =
  testCase "eq" $ do
    let assertNotEqual :: SAltID a -> SAltID a -> Assertion
        assertNotEqual x y =
          assertBool
            ("expected " <> show x <> " and " <> show y <> " to not be equal")
            (x /= y)
    let a :: SAltID "foo"
        a =
          SAltID $
            Param
              { paramName = CaseInsensitiveUpper (symbolSing @"ALTID"),
                paramValue = SParamValue (symbolSing @"foo")
              }

        b :: SAltID "foo"
        b =
          SAltID $
            Param
              { paramName = CaseInsensitiveUpper (symbolSing @"AltID"),
                paramValue = SParamValue (symbolSing @"foo")
              }

        c :: SAltID "foo"
        c =
          SAltID $
            Param
              { paramName = CaseInsensitiveUpper (symbolSing @"ALTID"),
                paramValue = SParamValue (symbolSing @"\"foo\"")
              }

    assertNotEqual a b
    assertNotEqual b c
    assertNotEqual a c

test_SAltID_parse :: TestTree
test_SAltID_parse =
  testCase "parse" $ do
    parse @(SAltID "") "ALTID="
      @?= Just
        ( SAltID $
            Param
              { paramName = CaseInsensitiveUpper (symbolSing @"ALTID"),
                paramValue = SParamValue (symbolSing @"")
              }
        )

    parse @(SAltID "foo") "ALTID=foo"
      @?= Just
        ( SAltID $
            Param
              { paramName = CaseInsensitiveUpper (symbolSing @"ALTID"),
                paramValue = SParamValue (symbolSing @"foo")
              }
        )

    parse @(SAltID "foo") "ALTID=\"foo\""
      @?= Just
        ( SAltID $
            Param
              { paramName = CaseInsensitiveUpper (symbolSing @"ALTID"),
                paramValue = SParamValue (symbolSing @"\"foo\"")
              }
        )

    parse @(SAltID "foo;") "ALTID=\"foo;\""
      @?= Just
        ( SAltID $
            Param
              { paramName = CaseInsensitiveUpper (symbolSing @"ALTID"),
                paramValue = SParamValue (symbolSing @"\"foo;\"")
              }
        )

    parse @(SAltID "foo;") "ALTID=foo;" @?= Nothing
    parse @(SAltID "foo,bar") "ALTID=foo,bar" @?= Nothing
    parse @(SAltID "\"foo\"") "ALTID=\"foo\"" @?= Nothing

test_SAltID_serialize :: TestTree
test_SAltID_serialize =
  testCase "serialize" $ do
    serialize @(SAltID "")
      ( SAltID $
          Param
            { paramName = CaseInsensitiveUpper (symbolSing @"ALTID"),
              paramValue = SParamValue (symbolSing @"")
            }
      )
      @?= "ALTID="

    serialize @(SAltID "foo")
      ( SAltID $
          Param
            { paramName = CaseInsensitiveUpper (symbolSing @"ALTID"),
              paramValue = SParamValue (symbolSing @"foo")
            }
      )
      @?= "ALTID=foo"

    serialize @(SAltID "foo")
      ( SAltID $
          Param
            { paramName = CaseInsensitiveUpper (symbolSing @"ALTID"),
              paramValue = SParamValue (symbolSing @"\"foo\"")
            }
      )
      @?= "ALTID=\"foo\""

    serialize @(SAltID "foo;")
      ( SAltID $
          Param
            { paramName = CaseInsensitiveUpper (symbolSing @"ALTID"),
              paramValue = SParamValue (symbolSing @"\"foo;\"")
            }
      )
      @?= "ALTID=\"foo;\""

test_SomeAltID :: TestTree
test_SomeAltID =
  testGroup
    "SomeAltID"
    [ test_SomeAltID_eq,
      test_SomeAltID_parse,
      test_SomeAltID_serialize
    ]

test_SomeAltID_eq :: TestTree
test_SomeAltID_eq =
  testCase "eq" $ do
    let assertNotEqual :: SomeAltID -> SomeAltID -> Assertion
        assertNotEqual x y =
          assertBool
            ("expected " <> show x <> " and " <> show y <> " to not be equal")
            (x /= y)
    let a :: SomeAltID
        a =
          SomeAltID . SAltID $
            Param
              { paramName = CaseInsensitiveUpper (symbolSing @"ALTID"),
                paramValue = SParamValue (symbolSing @"foo")
              }

        b :: SomeAltID
        b =
          SomeAltID . SAltID $
            Param
              { paramName = CaseInsensitiveUpper (symbolSing @"AltID"),
                paramValue = SParamValue (symbolSing @"foo")
              }

        c :: SomeAltID
        c =
          SomeAltID . SAltID $
            Param
              { paramName = CaseInsensitiveUpper (symbolSing @"ALTID"),
                paramValue = SParamValue (symbolSing @"\"foo\"")
              }

        d :: SomeAltID
        d =
          SomeAltID . SAltID $
            Param
              { paramName = CaseInsensitiveUpper (symbolSing @"ALTID"),
                paramValue = SParamValue (symbolSing @"\"foo;\"")
              }

    assertNotEqual a b
    assertNotEqual a c
    assertNotEqual a d
    assertNotEqual b c
    assertNotEqual b d
    assertNotEqual c d

test_SomeAltID_parse :: TestTree
test_SomeAltID_parse =
  testCase "parse" $ do
    parse @SomeAltID "ALTID="
      @?= Just
        ( SomeAltID . SAltID $
            Param
              { paramName = CaseInsensitiveUpper (symbolSing @"ALTID"),
                paramValue = SParamValue (symbolSing @"")
              }
        )

    parse @SomeAltID "ALTID=foo"
      @?= Just
        ( SomeAltID . SAltID $
            Param
              { paramName = CaseInsensitiveUpper (symbolSing @"ALTID"),
                paramValue = SParamValue (symbolSing @"foo")
              }
        )

    parse @SomeAltID "ALTID=\"foo\""
      @?= Just
        ( SomeAltID . SAltID $
            Param
              { paramName = CaseInsensitiveUpper (symbolSing @"ALTID"),
                paramValue = SParamValue (symbolSing @"\"foo\"")
              }
        )

    parse @SomeAltID "ALTID=\"foo;\""
      @?= Just
        ( SomeAltID . SAltID $
            Param
              { paramName = CaseInsensitiveUpper (symbolSing @"ALTID"),
                paramValue = SParamValue (symbolSing @"\"foo;\"")
              }
        )

    parse @SomeAltID "ALTID=foo;" @?= Nothing
    parse @SomeAltID "ALTID=foo,bar" @?= Nothing

test_SomeAltID_serialize :: TestTree
test_SomeAltID_serialize =
  testCase "serialize" $ do
    serialize
      ( SomeAltID . SAltID $
          Param
            { paramName = CaseInsensitiveUpper (symbolSing @"ALTID"),
              paramValue = SParamValue (symbolSing @"")
            }
      )
      @?= "ALTID="

    serialize
      ( SomeAltID . SAltID $
          Param
            { paramName = CaseInsensitiveUpper (symbolSing @"ALTID"),
              paramValue = SParamValue (symbolSing @"foo")
            }
      )
      @?= "ALTID=foo"

    serialize
      ( SomeAltID . SAltID $
          Param
            { paramName = CaseInsensitiveUpper (symbolSing @"ALTID"),
              paramValue = SParamValue (symbolSing @"\"foo\"")
            }
      )
      @?= "ALTID=\"foo\""

    serialize
      ( SomeAltID . SAltID $
          Param
            { paramName = CaseInsensitiveUpper (symbolSing @"ALTID"),
              paramValue = SParamValue (symbolSing @"\"foo;\"")
            }
      )
      @?= "ALTID=\"foo;\""
