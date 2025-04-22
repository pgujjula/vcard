-- SPDX-FileCopyrightText: Copyright Preetham Gujjula
-- SPDX-License-Identifier: BSD-3-Clause
{-# LANGUAGE OverloadedStrings #-}

module Test.VCard.Types.Param.Any (tests) where

import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.List.NonEmpty qualified as NonEmpty
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))
import VCard.Parse (parse)
import VCard.Serialize (serialize)
import VCard.Types.Param.Any (AnyParam (..))
import VCard.Types.Param.Generic (GenericParam (..))
import VCard.Types.Param.ParamValue (SParamValue (..), paramValueVal)
import VCard.Types.Textual (CaseInsensitiveUpper (..))
import VCard.Util.Symbol (symbolSing)

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
    parse @AnyParam "x-foo="
      @?= Just
        ( AnyParam
            ( GenericParam
                { genericParamName = CaseInsensitiveUpper (symbolSing @"x-foo"),
                  genericParamValue =
                    NonEmpty.singleton $
                      paramValueVal (SParamValue (symbolSing @""))
                }
            )
        )
    parse "X-FOO=bar"
      @?= Just
        ( AnyParam
            ( GenericParam
                { genericParamName = CaseInsensitiveUpper (symbolSing @"X-FOO"),
                  genericParamValue =
                    NonEmpty.singleton
                      (paramValueVal (SParamValue (symbolSing @"bar")))
                }
            )
        )
    parse "x-foo-12bar= alice,,\"\",\"bob,carter;\",dave"
      @?= Just
        ( AnyParam
            ( GenericParam
                { genericParamName = CaseInsensitiveUpper (symbolSing @"x-foo-12bar"),
                  genericParamValue =
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

    parse @AnyParam "x-foo =bar" @?= Nothing
    parse @AnyParam "x-=bar" @?= Nothing
    parse @AnyParam "xfoo=bar" @?= Nothing
    parse @AnyParam "foo=bar" @?= Nothing

test_serialize :: TestTree
test_serialize =
  testCase "serialize" $ do
    serialize
      ( AnyParam
          ( GenericParam
              { genericParamName = CaseInsensitiveUpper (symbolSing @"x-foo"),
                genericParamValue =
                  NonEmpty.singleton $
                    paramValueVal (SParamValue (symbolSing @""))
              }
          )
      )
      @?= "x-foo="

    serialize
      ( AnyParam
          ( GenericParam
              { genericParamName = CaseInsensitiveUpper (symbolSing @"X-FOO"),
                genericParamValue =
                  NonEmpty.singleton
                    (paramValueVal (SParamValue (symbolSing @"bar")))
              }
          )
      )
      @?= "X-FOO=bar"

    serialize
      ( AnyParam
          ( GenericParam
              { genericParamName = CaseInsensitiveUpper (symbolSing @"x-foo-12bar"),
                genericParamValue =
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
