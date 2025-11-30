-- SPDX-FileCopyrightText: Copyright Preetham Gujjula
-- SPDX-License-Identifier: BSD-3-Clause

module Test.VCard.Types.Property.ParamMap (tests) where

import Data.Dependent.Map qualified as DMap
import Data.Dependent.Sum (DSum ((:=>)))
import Data.Finite (finite)
import Data.Functor.Identity (Identity (..))
import Data.GADT.Compare (GCompare (gcompare), GEq (geq), GOrdering (..))
import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.List.NonEmpty qualified as NonEmpty
import Data.Type.Equality ((:~:) (Refl))
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))
import VCard.Types.Param
  ( GenericParam (..),
    PIDParam,
    PIDValue (..),
    PrefParam,
    PrefValue (..),
    genericParamName,
    genericParamValue,
    pidValueDecimalPart,
    pidValueWholePart,
  )
import VCard.Types.Property.ParamMap
  ( EmailParamMap,
    EmailParamTag (..),
    delete,
    empty,
    insert,
    lookup,
    unEmailParamMap,
  )
import VCard.Types.Textual (CaseInsensitiveUpper (..))
import VCard.Util.Symbol (symbolSing)
import Prelude hiding (lookup)

tests :: TestTree
tests =
  testGroup
    "ParamMap"
    [ test_EmailParamTag,
      test_EmailParamMap
    ]

test_EmailParamTag :: TestTree
test_EmailParamTag =
  testGroup
    "EmailParamTag"
    [ test_EmailParamTag_geq,
      test_EmailParamTag_gcompare,
      test_EmailParamTag_eq,
      test_EmailParamTag_compare
    ]

test_EmailParamTag_geq :: TestTree
test_EmailParamTag_geq = testCase "geq" $ do
  geq PIDParamTag PIDParamTag @?= Just Refl
  geq PIDParamTag PrefParamTag @?= Nothing
  geq PrefParamTag PIDParamTag @?= Nothing
  geq PrefParamTag PrefParamTag @?= Just Refl

test_EmailParamTag_gcompare :: TestTree
test_EmailParamTag_gcompare = testCase "gcompare" $ do
  gcompare PIDParamTag PIDParamTag @?= GEQ
  gcompare PIDParamTag PrefParamTag @?= GLT
  gcompare PrefParamTag PIDParamTag @?= GGT
  gcompare PrefParamTag PrefParamTag @?= GEQ

test_EmailParamTag_eq :: TestTree
test_EmailParamTag_eq = testCase "(==)" $ do
  PIDParamTag == PIDParamTag @?= True
  PrefParamTag == PrefParamTag @?= True

test_EmailParamTag_compare :: TestTree
test_EmailParamTag_compare = testCase "compare" $ do
  compare PIDParamTag PIDParamTag @?= EQ
  compare PrefParamTag PrefParamTag @?= EQ

test_EmailParamMap :: TestTree
test_EmailParamMap =
  testGroup
    "EmailParamMap"
    [ test_EmailParamMap_empty,
      test_EmailParamMap_insert,
      test_EmailParamMap_delete,
      test_EmailParamMap_lookup
    ]

test_EmailParamMap_empty :: TestTree
test_EmailParamMap_empty =
  testCase "empty" $ unwrap empty @?= []

test_EmailParamMap_insert :: TestTree
test_EmailParamMap_insert =
  testGroup
    "insert"
    [ testCase "single element" $ do
        unwrap pidParam1Map
          @?= [PIDParamTag :=> Identity pidParam1]

        unwrap prefParamMap
          @?= [PrefParamTag :=> Identity prefParam],
      testCase "multiple elements" $ do
        let mp1 = insert PrefParamTag prefParam pidParam1Map
            mp2 = insert PIDParamTag pidParam1 prefParamMap
            result =
              [ PIDParamTag :=> Identity pidParam1,
                PrefParamTag :=> Identity prefParam
              ]
        unwrap mp1 @?= result
        unwrap mp2 @?= result,
      testCase "overwrite elements" $
        unwrap (insert PIDParamTag pidParam2 allParamsMap)
          @?= [ PIDParamTag :=> Identity pidParam2,
                PrefParamTag :=> Identity prefParam
              ]
    ]

test_EmailParamMap_delete :: TestTree
test_EmailParamMap_delete =
  testGroup
    "delete"
    [ testCase "empty" $ do
        delete PIDParamTag empty @?= empty
        delete PrefParamTag empty @?= empty,
      testCase "singleton" $ do
        delete PIDParamTag pidParam1Map @?= empty
        delete PrefParamTag pidParam1Map @?= pidParam1Map,
      testCase "multiple elements" $ do
        delete PrefParamTag allParamsMap @?= pidParam1Map
        delete PIDParamTag allParamsMap @?= prefParamMap
    ]

test_EmailParamMap_lookup :: TestTree
test_EmailParamMap_lookup =
  testGroup
    "lookup"
    [ testCase "empty" $ do
        lookup PrefParamTag empty @?= Nothing
        lookup PIDParamTag empty @?= Nothing,
      testCase "singleton" $ do
        lookup PIDParamTag pidParam1Map @?= Just pidParam1
        lookup PrefParamTag pidParam1Map @?= Nothing
        lookup PIDParamTag prefParamMap @?= Nothing
        lookup PrefParamTag prefParamMap @?= Just prefParam,
      testCase "multiple elements" $ do
        lookup PIDParamTag allParamsMap @?= Just pidParam1
        lookup PrefParamTag allParamsMap @?= Just prefParam
    ]

--
-- Test values
--

pidParam1 :: PIDParam
pidParam1 =
  GenericParam
    { genericParamName = CaseInsensitiveUpper (symbolSing @"PID"),
      genericParamValue =
        NonEmpty.singleton $
          PIDValue
            { pidValueWholePart = NonEmpty.singleton 3,
              pidValueDecimalPart = Just (1 :| [4, 1, 5, 9])
            }
    }

pidParam2 :: PIDParam
pidParam2 =
  GenericParam
    { genericParamName = CaseInsensitiveUpper (symbolSing @"PID"),
      genericParamValue =
        NonEmpty.singleton $
          PIDValue
            { pidValueWholePart = NonEmpty.singleton 2,
              pidValueDecimalPart = Just (7 :| [1, 8, 2, 8])
            }
    }

prefParam :: PrefParam
prefParam =
  GenericParam
    { genericParamName = CaseInsensitiveUpper (symbolSing @"PREF"),
      genericParamValue = PrefValue (finite 10)
    }

pidParam1Map :: EmailParamMap
pidParam1Map = insert PIDParamTag pidParam1 empty

prefParamMap :: EmailParamMap
prefParamMap = insert PrefParamTag prefParam empty

allParamsMap :: EmailParamMap
allParamsMap = insert PrefParamTag prefParam pidParam1Map

--
-- Utilities
--

unwrap :: EmailParamMap -> [DSum EmailParamTag Identity]
unwrap = DMap.toList . unEmailParamMap
