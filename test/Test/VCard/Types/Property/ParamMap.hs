-- SPDX-FileCopyrightText: Copyright Preetham Gujjula
-- SPDX-License-Identifier: BSD-3-Clause

module Test.VCard.Types.Property.ParamMap (tests) where

import Data.Dependent.Map qualified as DMap
import Data.Dependent.Sum (DSum ((:=>)))
import Data.Finite (finite)
import Data.Function ((&))
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
    ParamValue,
    PrefParam,
    PrefValue (..),
    SParamValue (..),
    genericParamName,
    genericParamValue,
    paramValueVal,
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
  geq PIDParamTag (AnyParamTag (symbolSing @"X-FOO")) @?= Nothing

  geq PrefParamTag PIDParamTag @?= Nothing
  geq PrefParamTag PrefParamTag @?= Just Refl
  geq PrefParamTag (AnyParamTag (symbolSing @"X-FOO")) @?= Nothing

  geq (AnyParamTag (symbolSing @"X-FOO")) PIDParamTag @?= Nothing
  geq (AnyParamTag (symbolSing @"X-FOO")) PrefParamTag @?= Nothing
  geq (AnyParamTag (symbolSing @"X-FOO")) (AnyParamTag (symbolSing @"X-BAR"))
    @?= Nothing
  geq (AnyParamTag (symbolSing @"X-FOO")) (AnyParamTag (symbolSing @"X-FOO"))
    @?= Just Refl

test_EmailParamTag_gcompare :: TestTree
test_EmailParamTag_gcompare = testCase "gcompare" $ do
  gcompare PIDParamTag PIDParamTag @?= GEQ
  gcompare PIDParamTag PrefParamTag @?= GLT
  gcompare PIDParamTag (AnyParamTag (symbolSing @"X-FOO")) @?= GLT

  gcompare PrefParamTag PIDParamTag @?= GGT
  gcompare PrefParamTag PrefParamTag @?= GEQ
  gcompare PrefParamTag (AnyParamTag (symbolSing @"X-FOO")) @?= GLT

  gcompare (AnyParamTag (symbolSing @"X-FOO")) PIDParamTag @?= GGT
  gcompare (AnyParamTag (symbolSing @"X-FOO")) PrefParamTag @?= GGT
  gcompare (AnyParamTag (symbolSing @"X-FOO")) (AnyParamTag (symbolSing @"X-BAR")) @?= GGT
  gcompare (AnyParamTag (symbolSing @"X-BAR")) (AnyParamTag (symbolSing @"X-FOO")) @?= GLT
  gcompare (AnyParamTag (symbolSing @"X-FOO")) (AnyParamTag (symbolSing @"X-FOO")) @?= GEQ

test_EmailParamTag_eq :: TestTree
test_EmailParamTag_eq = testCase "(==)" $ do
  PIDParamTag == PIDParamTag @?= True
  PrefParamTag == PrefParamTag @?= True
  AnyParamTag (symbolSing @"X-FOO") == AnyParamTag (symbolSing @"X-FOO")
    @?= True

test_EmailParamTag_compare :: TestTree
test_EmailParamTag_compare = testCase "compare" $ do
  compare PIDParamTag PIDParamTag @?= EQ
  compare PrefParamTag PrefParamTag @?= EQ
  compare
    (AnyParamTag (symbolSing @"X-FOO"))
    (AnyParamTag (symbolSing @"X-FOO"))
    @?= EQ

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
        unwrap pidParam1Map @?= [PIDParamTag :=> Identity pidParam1]
        unwrap prefParamMap @?= [PrefParamTag :=> Identity prefParam]
        unwrap anyParamFoo1Map
          @?= [AnyParamTag (symbolSing @"X-FOO") :=> Identity anyParamFoo1]
        unwrap anyParamFoo2Map
          @?= [AnyParamTag (symbolSing @"X-FOO") :=> Identity anyParamFoo2]
        unwrap anyParamBarMap
          @?= [AnyParamTag (symbolSing @"X-BAR") :=> Identity anyParamBar],
      testCase "multiple elements" $ do
        let mp1 =
              empty
                & insert PIDParamTag pidParam1
                & insert PrefParamTag prefParam
                & insert (AnyParamTag (symbolSing @"X-FOO")) anyParamFoo1
                & insert (AnyParamTag (symbolSing @"X-BAR")) anyParamBar
            mp2 =
              empty
                & insert (AnyParamTag (symbolSing @"X-BAR")) anyParamBar
                & insert (AnyParamTag (symbolSing @"X-FOO")) anyParamFoo1
                & insert PrefParamTag prefParam
                & insert PIDParamTag pidParam1
            result =
              [ PIDParamTag :=> Identity pidParam1,
                PrefParamTag :=> Identity prefParam,
                AnyParamTag (symbolSing @"X-BAR") :=> Identity anyParamBar,
                AnyParamTag (symbolSing @"X-FOO") :=> Identity anyParamFoo1
              ]
        unwrap mp1 @?= result
        unwrap mp2 @?= result,
      testCase "overwrite elements" $
        let replacedMap =
              allParamsMap
                & insert PIDParamTag pidParam2
                & insert (AnyParamTag (symbolSing @"X-FOO")) anyParamFoo2
         in unwrap replacedMap
              @?= [ PIDParamTag :=> Identity pidParam2,
                    PrefParamTag :=> Identity prefParam,
                    AnyParamTag (symbolSing @"X-BAR") :=> Identity anyParamBar,
                    AnyParamTag (symbolSing @"X-FOO") :=> Identity anyParamFoo2
                  ]
    ]

test_EmailParamMap_delete :: TestTree
test_EmailParamMap_delete =
  testGroup
    "delete"
    [ testCase "empty" $ do
        delete PIDParamTag empty @?= empty
        delete PrefParamTag empty @?= empty
        delete (AnyParamTag (symbolSing @"X-FOO")) empty @?= empty
        delete (AnyParamTag (symbolSing @"X-BAR")) empty @?= empty,
      testCase "singleton" $ do
        delete PIDParamTag pidParam1Map @?= empty
        delete PrefParamTag pidParam1Map @?= pidParam1Map

        delete (AnyParamTag (symbolSing @"X-FOO")) anyParamFoo1Map @?= empty
        delete (AnyParamTag (symbolSing @"X-FOO")) anyParamFoo2Map @?= empty
        delete (AnyParamTag (symbolSing @"X-BAR")) anyParamFoo1Map @?= anyParamFoo1Map,
      testCase "multiple elements" $
        let deletedMap =
              allParamsMap
                & delete PIDParamTag
                & delete (AnyParamTag (symbolSing @"X-FOO"))
         in unwrap deletedMap
              @?= [ PrefParamTag :=> Identity prefParam,
                    AnyParamTag (symbolSing @"X-BAR") :=> Identity anyParamBar
                  ]
    ]

test_EmailParamMap_lookup :: TestTree
test_EmailParamMap_lookup =
  testGroup
    "lookup"
    [ testCase "empty" $ do
        lookup PrefParamTag empty @?= Nothing
        lookup PIDParamTag empty @?= Nothing
        lookup (AnyParamTag (symbolSing @"X-FOO")) empty @?= Nothing
        lookup (AnyParamTag (symbolSing @"X-BAR")) empty @?= Nothing,
      testCase "singleton" $ do
        lookup PIDParamTag pidParam1Map @?= Just pidParam1
        lookup PrefParamTag pidParam1Map @?= Nothing
        lookup (AnyParamTag (symbolSing @"X-FOO")) pidParam1Map @?= Nothing

        lookup PIDParamTag prefParamMap @?= Nothing
        lookup PrefParamTag prefParamMap @?= Just prefParam
        lookup (AnyParamTag (symbolSing @"X-FOO")) prefParamMap @?= Nothing

        lookup PIDParamTag anyParamFoo1Map @?= Nothing
        lookup PrefParamTag anyParamFoo1Map @?= Nothing
        lookup (AnyParamTag (symbolSing @"X-FOO")) anyParamFoo1Map @?= Just anyParamFoo1
        lookup (AnyParamTag (symbolSing @"X-BAR")) anyParamFoo1Map @?= Nothing

        lookup PIDParamTag anyParamBarMap @?= Nothing
        lookup PrefParamTag anyParamBarMap @?= Nothing
        lookup (AnyParamTag (symbolSing @"X-FOO")) anyParamBarMap @?= Nothing
        lookup (AnyParamTag (symbolSing @"X-BAR")) anyParamBarMap @?= Just anyParamBar,
      testCase "multiple elements" $ do
        lookup PIDParamTag allParamsMap @?= Just pidParam1
        lookup PrefParamTag allParamsMap @?= Just prefParam
        lookup (AnyParamTag (symbolSing @"X-FOO")) allParamsMap @?= Just anyParamFoo1
        lookup (AnyParamTag (symbolSing @"X-BAR")) allParamsMap @?= Just anyParamBar
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

anyParamFoo1 :: GenericParam "X-FOO" (NonEmpty ParamValue)
anyParamFoo1 =
  GenericParam
    { genericParamName = CaseInsensitiveUpper (symbolSing @"X-FOO"),
      genericParamValue =
        NonEmpty.singleton (paramValueVal (SParamValue (symbolSing @"lorem")))
    }

anyParamFoo2 :: GenericParam "X-FOO" (NonEmpty ParamValue)
anyParamFoo2 =
  GenericParam
    { genericParamName = CaseInsensitiveUpper (symbolSing @"x-foo"),
      genericParamValue =
        NonEmpty.singleton (paramValueVal (SParamValue (symbolSing @"ipsum")))
    }

anyParamBar :: GenericParam "X-BAR" (NonEmpty ParamValue)
anyParamBar =
  GenericParam
    { genericParamName = CaseInsensitiveUpper (symbolSing @"X-BAR"),
      genericParamValue =
        paramValueVal (SParamValue (symbolSing @"dolor"))
          :| [ paramValueVal (SParamValue (symbolSing @"sit")),
               paramValueVal (SParamValue (symbolSing @"amet"))
             ]
    }

pidParam1Map :: EmailParamMap
pidParam1Map = insert PIDParamTag pidParam1 empty

prefParamMap :: EmailParamMap
prefParamMap = insert PrefParamTag prefParam empty

anyParamFoo1Map :: EmailParamMap
anyParamFoo1Map = insert (AnyParamTag (symbolSing @"X-FOO")) anyParamFoo1 empty

anyParamFoo2Map :: EmailParamMap
anyParamFoo2Map = insert (AnyParamTag (symbolSing @"X-FOO")) anyParamFoo2 empty

anyParamBarMap :: EmailParamMap
anyParamBarMap = insert (AnyParamTag (symbolSing @"X-BAR")) anyParamBar empty

allParamsMap :: EmailParamMap
allParamsMap =
  empty
    & insert PIDParamTag pidParam1
    & insert PrefParamTag prefParam
    & insert (AnyParamTag (symbolSing @"X-FOO")) anyParamFoo1
    & insert (AnyParamTag (symbolSing @"X-BAR")) anyParamBar

--
-- Utilities
--

unwrap :: EmailParamMap -> [DSum EmailParamTag Identity]
unwrap = DMap.toList . unEmailParamMap
