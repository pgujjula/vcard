-- SPDX-FileCopyrightText: Copyright Preetham Gujjula
-- SPDX-License-Identifier: BSD-3-Clause
{-# LANGUAGE OverloadedStrings #-}

module Test.VCard.Types.Param.Any (tests) where

import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.List.NonEmpty qualified as NonEmpty
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))
import VCard.CaseInsensitive (CaseInsensitiveUpper (..))
import VCard.Parse (parse)
import VCard.Serialize (serialize)
import VCard.Symbol.Private (symbolSing)
import VCard.Types.Param.Any (Any (..))
import VCard.Types.Param.Generic (Param (..))
import VCard.Types.Param.ParamValue (SParamValue (..), paramValueVal)

tests :: TestTree
tests =
  testGroup
    "Any"
    [ test_parse,
      test_serialize
    ]

test_parse :: TestTree
test_parse =
  testCase "parse" $ do
    parse @Any "x-foo="
      @?= Just
        ( Any
            ( Param
                { paramName = CaseInsensitiveUpper (symbolSing @"x-foo"),
                  paramValue =
                    NonEmpty.singleton $
                      paramValueVal (SParamValue (symbolSing @""))
                }
            )
        )
    parse "X-FOO=bar"
      @?= Just
        ( Any
            ( Param
                { paramName = CaseInsensitiveUpper (symbolSing @"X-FOO"),
                  paramValue =
                    NonEmpty.singleton
                      (paramValueVal (SParamValue (symbolSing @"bar")))
                }
            )
        )
    parse "x-foo-12bar= alice,,\"\",\"bob,carter;\",dave"
      @?= Just
        ( Any
            ( Param
                { paramName = CaseInsensitiveUpper (symbolSing @"x-foo-12bar"),
                  paramValue =
                    paramValueVal (SParamValue (symbolSing @" alice"))
                      :| [ paramValueVal (SParamValue (symbolSing @"")),
                           paramValueVal (SParamValue (symbolSing @"\"\"")),
                           paramValueVal
                             (SParamValue (symbolSing @"\"bob,carter;\"")),
                           paramValueVal (SParamValue (symbolSing @"dave"))
                         ]
                }
            )
        )

    parse @Any "x-foo =bar" @?= Nothing
    parse @Any "x-=bar" @?= Nothing
    parse @Any "xfoo=bar" @?= Nothing
    parse @Any "foo=bar" @?= Nothing

test_serialize :: TestTree
test_serialize =
  testCase "serialize" $ do
    serialize
      ( Any
          ( Param
              { paramName = CaseInsensitiveUpper (symbolSing @"x-foo"),
                paramValue =
                  NonEmpty.singleton $
                    paramValueVal (SParamValue (symbolSing @""))
              }
          )
      )
      @?= "x-foo="

    serialize
      ( Any
          ( Param
              { paramName = CaseInsensitiveUpper (symbolSing @"X-FOO"),
                paramValue =
                  NonEmpty.singleton
                    (paramValueVal (SParamValue (symbolSing @"bar")))
              }
          )
      )
      @?= "X-FOO=bar"

    serialize
      ( Any
          ( Param
              { paramName = CaseInsensitiveUpper (symbolSing @"x-foo-12bar"),
                paramValue =
                  paramValueVal (SParamValue (symbolSing @" alice"))
                    :| [ paramValueVal (SParamValue (symbolSing @"")),
                         paramValueVal (SParamValue (symbolSing @"\"\"")),
                         paramValueVal
                           (SParamValue (symbolSing @"\"bob,carter;\"")),
                         paramValueVal (SParamValue (symbolSing @"dave"))
                       ]
              }
          )
      )
      @?= "x-foo-12bar= alice,,\"\",\"bob,carter;\",dave"
