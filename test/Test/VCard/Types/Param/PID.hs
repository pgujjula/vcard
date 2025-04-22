-- SPDX-FileCopyrightText: Copyright Preetham Gujjula
-- SPDX-License-Identifier: BSD-3-Clause
{-# LANGUAGE OverloadedStrings #-}

module Test.VCard.Types.Param.PID (tests) where

import Control.Monad (forM_)
import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.List.NonEmpty qualified as NonEmpty
import Data.Proxy (Proxy (..))
import Data.Text (Text)
import Test.Tasty (TestName, TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))
import VCard.Parse (HasParser, parse)
import VCard.Serialize (HasSerializer, serialize)
import VCard.Types.Param.Generic (GenericParam (..))
import VCard.Types.Param.PID (PIDParam, PIDValue (..))
import VCard.Types.Textual (CaseInsensitiveUpper (..))
import VCard.Util.Symbol (symbolSing)

{-# ANN module ("HLint: ignore Use camelCase" :: String) #-}

tests :: TestTree
tests =
  testGroup
    "PID"
    [ test_parse,
      test_serialize
    ]

test_parse :: TestTree
test_parse =
  testGroup
    "parse"
    [ testParseValid cases_valid,
      testParseInvalid (Proxy @PIDParam) cases_invalid
    ]

test_serialize :: TestTree
test_serialize = testSerialize "serialize" cases_valid

cases_valid :: [(Text, PIDParam)]
cases_valid =
  [ ( "PID=0",
      GenericParam
        { genericParamName = CaseInsensitiveUpper (symbolSing @"PID"),
          genericParamValue =
            NonEmpty.singleton $
              PIDValue
                { pidValueWholePart = NonEmpty.singleton 0,
                  pidValueDecimalPart = Nothing
                }
        }
    ),
    ( "pid=1,5,10,52,200,419",
      GenericParam
        { genericParamName = CaseInsensitiveUpper (symbolSing @"pid"),
          genericParamValue =
            PIDValue
              { pidValueWholePart = 1 :| [],
                pidValueDecimalPart = Nothing
              }
              :| [ PIDValue
                     { pidValueWholePart = 5 :| [],
                       pidValueDecimalPart = Nothing
                     },
                   PIDValue
                     { pidValueWholePart = 1 :| [0],
                       pidValueDecimalPart = Nothing
                     },
                   PIDValue
                     { pidValueWholePart = 5 :| [2],
                       pidValueDecimalPart = Nothing
                     },
                   PIDValue
                     { pidValueWholePart = 2 :| [0, 0],
                       pidValueDecimalPart = Nothing
                     },
                   PIDValue
                     { pidValueWholePart = 4 :| [1, 9],
                       pidValueDecimalPart = Nothing
                     }
                 ]
        }
    ),
    ( "Pid=00,0401,0009250,0.0,0.1,1.4",
      GenericParam
        { genericParamName = CaseInsensitiveUpper (symbolSing @"Pid"),
          genericParamValue =
            PIDValue
              { pidValueWholePart = 0 :| [0],
                pidValueDecimalPart = Nothing
              }
              :| [ PIDValue
                     { pidValueWholePart = 0 :| [4, 0, 1],
                       pidValueDecimalPart = Nothing
                     },
                   PIDValue
                     { pidValueWholePart = 0 :| [0, 0, 9, 2, 5, 0],
                       pidValueDecimalPart = Nothing
                     },
                   PIDValue
                     { pidValueWholePart = 0 :| [],
                       pidValueDecimalPart = Just (0 :| [])
                     },
                   PIDValue
                     { pidValueWholePart = 0 :| [],
                       pidValueDecimalPart = Just (1 :| [])
                     },
                   PIDValue
                     { pidValueWholePart = 1 :| [],
                       pidValueDecimalPart = Just (4 :| [])
                     }
                 ]
        }
    ),
    ( "pId=12.55,63.0098,83.27000,005310.021900",
      GenericParam
        { genericParamName = CaseInsensitiveUpper (symbolSing @"pId"),
          genericParamValue =
            PIDValue
              { pidValueWholePart = 1 :| [2],
                pidValueDecimalPart = Just (5 :| [5])
              }
              :| [ PIDValue
                     { pidValueWholePart = 6 :| [3],
                       pidValueDecimalPart = Just (0 :| [0, 9, 8])
                     },
                   PIDValue
                     { pidValueWholePart = 8 :| [3],
                       pidValueDecimalPart = Just (2 :| [7, 0, 0, 0])
                     },
                   PIDValue
                     { pidValueWholePart = 0 :| [0, 5, 3, 1, 0],
                       pidValueDecimalPart = Just (0 :| [2, 1, 9, 0, 0])
                     }
                 ]
        }
    )
  ]

cases_invalid :: [Text]
cases_invalid =
  ["PID=", "PID=1.", "PID= 1", "PID =1", "PID=1;", "PID=1:"]
    ++ ["PID=,", "PID=1,,", "PID=,1,"]

--------------------
-- Testing functions
--------------------

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
