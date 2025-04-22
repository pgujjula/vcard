-- SPDX-FileCopyrightText: Copyright Preetham Gujjula
-- SPDX-License-Identifier: BSD-3-Clause
{-# LANGUAGE OverloadedStrings #-}

module Test.VCard.Types.Param.AltID (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (Assertion, assertBool, testCase, (@?=))
import VCard.Parse (parse)
import VCard.Serialize (serialize)
import VCard.Types.Param.AltID
  ( AltIDParam,
    SAltIDParam (..),
    SomeAltIDParam (..),
    altIDVal,
    someAltIDVal,
  )
import VCard.Types.Param.Generic (GenericParam (..))
import VCard.Types.Param.ParamValue (SParamValue (..), paramValueVal)
import VCard.Types.Textual (CaseInsensitiveUpper (..))
import VCard.Util.Symbol (symbolSing)

tests :: TestTree
tests =
  testGroup
    "AltID"
    [ test_altIDVal,
      test_someAltIDVal,
      test_AltIDParam,
      test_SAltIDParam,
      test_SomeAltIDParam
    ]

test_altIDVal :: TestTree
test_altIDVal =
  testCase "altIDVal" $
    let param =
          GenericParam
            { genericParamName = CaseInsensitiveUpper (symbolSing @"ALTID"),
              genericParamValue = SParamValue (symbolSing @"foo")
            }
        saltid = SAltIDParam param
        altID =
          GenericParam
            { genericParamName = CaseInsensitiveUpper (symbolSing @"ALTID"),
              genericParamValue = paramValueVal (SParamValue (symbolSing @"foo"))
            }
     in altIDVal saltid @?= altID

test_someAltIDVal :: TestTree
test_someAltIDVal =
  testCase "someAltIDVal" $ do
    let altID =
          GenericParam
            { genericParamName = CaseInsensitiveUpper (symbolSing @"ALTID"),
              genericParamValue = paramValueVal (SParamValue (symbolSing @"foo"))
            }
        someAltID =
          SomeAltIDParam . SAltIDParam $
            GenericParam
              { genericParamName = CaseInsensitiveUpper (symbolSing @"ALTID"),
                genericParamValue = SParamValue (symbolSing @"foo")
              }
     in someAltIDVal altID @?= someAltID

test_AltIDParam :: TestTree
test_AltIDParam =
  testGroup
    "AltIDParam"
    [ test_AltIDParam_parse,
      test_AltIDParam_serialize
    ]

test_AltIDParam_parse :: TestTree
test_AltIDParam_parse =
  testCase "parse" $ do
    parse @AltIDParam "ALTID="
      @?= Just
        ( GenericParam
            { genericParamName = CaseInsensitiveUpper (symbolSing @"ALTID"),
              genericParamValue = paramValueVal (SParamValue (symbolSing @""))
            }
        )

    parse @AltIDParam "ALTID=foo"
      @?= Just
        ( GenericParam
            { genericParamName = CaseInsensitiveUpper (symbolSing @"ALTID"),
              genericParamValue = paramValueVal (SParamValue (symbolSing @"foo"))
            }
        )

    parse @AltIDParam "ALTID=\"foo\""
      @?= Just
        ( GenericParam
            { genericParamName = CaseInsensitiveUpper (symbolSing @"ALTID"),
              genericParamValue = paramValueVal (SParamValue (symbolSing @"\"foo\""))
            }
        )

    parse @AltIDParam "ALTID=foo;" @?= Nothing
    parse @AltIDParam "ALTID=foo,bar" @?= Nothing

test_AltIDParam_serialize :: TestTree
test_AltIDParam_serialize =
  testCase "serialize" $ do
    serialize @AltIDParam
      ( GenericParam
          { genericParamName = CaseInsensitiveUpper (symbolSing @"ALTID"),
            genericParamValue = paramValueVal (SParamValue (symbolSing @""))
          }
      )
      @?= "ALTID="

    serialize
      ( GenericParam
          { genericParamName = CaseInsensitiveUpper (symbolSing @"ALTID"),
            genericParamValue = paramValueVal (SParamValue (symbolSing @"foo"))
          }
      )
      @?= "ALTID=foo"

    serialize
      ( GenericParam
          { genericParamName = CaseInsensitiveUpper (symbolSing @"ALTID"),
            genericParamValue = paramValueVal (SParamValue (symbolSing @"\"foo\""))
          }
      )
      @?= "ALTID=\"foo\""

test_SAltIDParam :: TestTree
test_SAltIDParam =
  testGroup
    "SAltIDParam"
    [ test_SAltIDParam_Eq,
      test_SAltIDParam_parse,
      test_SAltIDParam_serialize
    ]

test_SAltIDParam_Eq :: TestTree
test_SAltIDParam_Eq =
  testCase "eq" $ do
    let assertNotEqual :: SAltIDParam a -> SAltIDParam a -> Assertion
        assertNotEqual x y =
          assertBool
            ("expected " <> show x <> " and " <> show y <> " to not be equal")
            (x /= y)
    let a :: SAltIDParam "foo"
        a =
          SAltIDParam $
            GenericParam
              { genericParamName = CaseInsensitiveUpper (symbolSing @"ALTID"),
                genericParamValue = SParamValue (symbolSing @"foo")
              }

        b :: SAltIDParam "foo"
        b =
          SAltIDParam $
            GenericParam
              { genericParamName = CaseInsensitiveUpper (symbolSing @"AltID"),
                genericParamValue = SParamValue (symbolSing @"foo")
              }

        c :: SAltIDParam "foo"
        c =
          SAltIDParam $
            GenericParam
              { genericParamName = CaseInsensitiveUpper (symbolSing @"ALTID"),
                genericParamValue = SParamValue (symbolSing @"\"foo\"")
              }

    assertNotEqual a b
    assertNotEqual b c
    assertNotEqual a c

test_SAltIDParam_parse :: TestTree
test_SAltIDParam_parse =
  testCase "parse" $ do
    parse @(SAltIDParam "") "ALTID="
      @?= Just
        ( SAltIDParam $
            GenericParam
              { genericParamName = CaseInsensitiveUpper (symbolSing @"ALTID"),
                genericParamValue = SParamValue (symbolSing @"")
              }
        )

    parse @(SAltIDParam "foo") "ALTID=foo"
      @?= Just
        ( SAltIDParam $
            GenericParam
              { genericParamName = CaseInsensitiveUpper (symbolSing @"ALTID"),
                genericParamValue = SParamValue (symbolSing @"foo")
              }
        )

    parse @(SAltIDParam "foo") "ALTID=\"foo\""
      @?= Just
        ( SAltIDParam $
            GenericParam
              { genericParamName = CaseInsensitiveUpper (symbolSing @"ALTID"),
                genericParamValue = SParamValue (symbolSing @"\"foo\"")
              }
        )

    parse @(SAltIDParam "foo;") "ALTID=\"foo;\""
      @?= Just
        ( SAltIDParam $
            GenericParam
              { genericParamName = CaseInsensitiveUpper (symbolSing @"ALTID"),
                genericParamValue = SParamValue (symbolSing @"\"foo;\"")
              }
        )

    parse @(SAltIDParam "foo;") "ALTID=foo;" @?= Nothing
    parse @(SAltIDParam "foo,bar") "ALTID=foo,bar" @?= Nothing
    parse @(SAltIDParam "\"foo\"") "ALTID=\"foo\"" @?= Nothing

test_SAltIDParam_serialize :: TestTree
test_SAltIDParam_serialize =
  testCase "serialize" $ do
    serialize @(SAltIDParam "")
      ( SAltIDParam $
          GenericParam
            { genericParamName = CaseInsensitiveUpper (symbolSing @"ALTID"),
              genericParamValue = SParamValue (symbolSing @"")
            }
      )
      @?= "ALTID="

    serialize @(SAltIDParam "foo")
      ( SAltIDParam $
          GenericParam
            { genericParamName = CaseInsensitiveUpper (symbolSing @"ALTID"),
              genericParamValue = SParamValue (symbolSing @"foo")
            }
      )
      @?= "ALTID=foo"

    serialize @(SAltIDParam "foo")
      ( SAltIDParam $
          GenericParam
            { genericParamName = CaseInsensitiveUpper (symbolSing @"ALTID"),
              genericParamValue = SParamValue (symbolSing @"\"foo\"")
            }
      )
      @?= "ALTID=\"foo\""

    serialize @(SAltIDParam "foo;")
      ( SAltIDParam $
          GenericParam
            { genericParamName = CaseInsensitiveUpper (symbolSing @"ALTID"),
              genericParamValue = SParamValue (symbolSing @"\"foo;\"")
            }
      )
      @?= "ALTID=\"foo;\""

test_SomeAltIDParam :: TestTree
test_SomeAltIDParam =
  testGroup
    "SomeAltIDParam"
    [ test_SomeAltIDParam_eq,
      test_SomeAltIDParam_parse,
      test_SomeAltIDParam_serialize
    ]

test_SomeAltIDParam_eq :: TestTree
test_SomeAltIDParam_eq =
  testCase "eq" $ do
    let assertNotEqual :: SomeAltIDParam -> SomeAltIDParam -> Assertion
        assertNotEqual x y =
          assertBool
            ("expected " <> show x <> " and " <> show y <> " to not be equal")
            (x /= y)
    let a :: SomeAltIDParam
        a =
          SomeAltIDParam . SAltIDParam $
            GenericParam
              { genericParamName = CaseInsensitiveUpper (symbolSing @"ALTID"),
                genericParamValue = SParamValue (symbolSing @"foo")
              }

        b :: SomeAltIDParam
        b =
          SomeAltIDParam . SAltIDParam $
            GenericParam
              { genericParamName = CaseInsensitiveUpper (symbolSing @"AltID"),
                genericParamValue = SParamValue (symbolSing @"foo")
              }

        c :: SomeAltIDParam
        c =
          SomeAltIDParam . SAltIDParam $
            GenericParam
              { genericParamName = CaseInsensitiveUpper (symbolSing @"ALTID"),
                genericParamValue = SParamValue (symbolSing @"\"foo\"")
              }

        d :: SomeAltIDParam
        d =
          SomeAltIDParam . SAltIDParam $
            GenericParam
              { genericParamName = CaseInsensitiveUpper (symbolSing @"ALTID"),
                genericParamValue = SParamValue (symbolSing @"\"foo;\"")
              }

    assertNotEqual a b
    assertNotEqual a c
    assertNotEqual a d
    assertNotEqual b c
    assertNotEqual b d
    assertNotEqual c d

test_SomeAltIDParam_parse :: TestTree
test_SomeAltIDParam_parse =
  testCase "parse" $ do
    parse @SomeAltIDParam "ALTID="
      @?= Just
        ( SomeAltIDParam . SAltIDParam $
            GenericParam
              { genericParamName = CaseInsensitiveUpper (symbolSing @"ALTID"),
                genericParamValue = SParamValue (symbolSing @"")
              }
        )

    parse @SomeAltIDParam "ALTID=foo"
      @?= Just
        ( SomeAltIDParam . SAltIDParam $
            GenericParam
              { genericParamName = CaseInsensitiveUpper (symbolSing @"ALTID"),
                genericParamValue = SParamValue (symbolSing @"foo")
              }
        )

    parse @SomeAltIDParam "ALTID=\"foo\""
      @?= Just
        ( SomeAltIDParam . SAltIDParam $
            GenericParam
              { genericParamName = CaseInsensitiveUpper (symbolSing @"ALTID"),
                genericParamValue = SParamValue (symbolSing @"\"foo\"")
              }
        )

    parse @SomeAltIDParam "ALTID=\"foo;\""
      @?= Just
        ( SomeAltIDParam . SAltIDParam $
            GenericParam
              { genericParamName = CaseInsensitiveUpper (symbolSing @"ALTID"),
                genericParamValue = SParamValue (symbolSing @"\"foo;\"")
              }
        )

    parse @SomeAltIDParam "ALTID=foo;" @?= Nothing
    parse @SomeAltIDParam "ALTID=foo,bar" @?= Nothing

test_SomeAltIDParam_serialize :: TestTree
test_SomeAltIDParam_serialize =
  testCase "serialize" $ do
    serialize
      ( SomeAltIDParam . SAltIDParam $
          GenericParam
            { genericParamName = CaseInsensitiveUpper (symbolSing @"ALTID"),
              genericParamValue = SParamValue (symbolSing @"")
            }
      )
      @?= "ALTID="

    serialize
      ( SomeAltIDParam . SAltIDParam $
          GenericParam
            { genericParamName = CaseInsensitiveUpper (symbolSing @"ALTID"),
              genericParamValue = SParamValue (symbolSing @"foo")
            }
      )
      @?= "ALTID=foo"

    serialize
      ( SomeAltIDParam . SAltIDParam $
          GenericParam
            { genericParamName = CaseInsensitiveUpper (symbolSing @"ALTID"),
              genericParamValue = SParamValue (symbolSing @"\"foo\"")
            }
      )
      @?= "ALTID=\"foo\""

    serialize
      ( SomeAltIDParam . SAltIDParam $
          GenericParam
            { genericParamName = CaseInsensitiveUpper (symbolSing @"ALTID"),
              genericParamValue = SParamValue (symbolSing @"\"foo;\"")
            }
      )
      @?= "ALTID=\"foo;\""
