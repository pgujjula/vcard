-- SPDX-FileCopyrightText: Copyright Preetham Gujjula
-- SPDX-License-Identifier: BSD-3-Clause
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module Test.VCard.Symbol.Private.Prefix (tests) where

import Data.Dynamic (Dynamic, toDyn)
import Data.Type.Equality ((:~:) (Refl))
import Test.Tasty (TestTree)
import Test.Tasty.HUnit (testCase)
import VCard.Symbol.Private.Prefix (IsPrefixOf, IsPrefixOfInsensitive)

tests :: TestTree
tests = testCase "Prefix" (seq test_Types (pure ()))

test_Types :: [Dynamic]
test_Types =
  concat
    [ test_IsPrefixOf,
      test_IsPrefixOfInsensitive
    ]

test_IsPrefixOf :: [Dynamic]
test_IsPrefixOf =
  [ --
    -- Regular tests
    --
    toDyn (Refl :: IsPrefixOf "" "" :~: 'True),
    toDyn (Refl :: IsPrefixOf "" "a" :~: 'True),
    toDyn (Refl :: IsPrefixOf "" "abc" :~: 'True),
    toDyn (Refl :: IsPrefixOf "" "abcdef" :~: 'True),
    --
    toDyn (Refl :: IsPrefixOf "a" "" :~: 'False),
    toDyn (Refl :: IsPrefixOf "a" "a" :~: 'True),
    toDyn (Refl :: IsPrefixOf "a" "abc" :~: 'True),
    toDyn (Refl :: IsPrefixOf "a" "abcdef" :~: 'True),
    --
    toDyn (Refl :: IsPrefixOf "abc" "" :~: 'False),
    toDyn (Refl :: IsPrefixOf "abc" "a" :~: 'False),
    toDyn (Refl :: IsPrefixOf "abc" "abc" :~: 'True),
    toDyn (Refl :: IsPrefixOf "abc" "abcdef" :~: 'True),
    --
    toDyn (Refl :: IsPrefixOf "abcdef" "" :~: 'False),
    toDyn (Refl :: IsPrefixOf "abcdef" "a" :~: 'False),
    toDyn (Refl :: IsPrefixOf "abcdef" "abc" :~: 'False),
    toDyn (Refl :: IsPrefixOf "abcdef" "abcdef" :~: 'True),
    --
    -- Insensitive tests
    --
    toDyn (Refl :: IsPrefixOf "a" "" :~: 'False),
    toDyn (Refl :: IsPrefixOf "a" "A" :~: 'False),
    toDyn (Refl :: IsPrefixOf "a" "ABC" :~: 'False),
    toDyn (Refl :: IsPrefixOf "a" "ABCDEF" :~: 'False),
    --
    toDyn (Refl :: IsPrefixOf "abc" "" :~: 'False),
    toDyn (Refl :: IsPrefixOf "abc" "A" :~: 'False),
    toDyn (Refl :: IsPrefixOf "abc" "ABC" :~: 'False),
    toDyn (Refl :: IsPrefixOf "abc" "ABCDEF" :~: 'False)
  ]

test_IsPrefixOfInsensitive :: [Dynamic]
test_IsPrefixOfInsensitive =
  [ --
    -- Regular tests
    --
    toDyn (Refl :: IsPrefixOf "" "" :~: 'True),
    toDyn (Refl :: IsPrefixOf "" "a" :~: 'True),
    toDyn (Refl :: IsPrefixOf "" "abc" :~: 'True),
    toDyn (Refl :: IsPrefixOf "" "abcdef" :~: 'True),
    --
    toDyn (Refl :: IsPrefixOf "a" "" :~: 'False),
    toDyn (Refl :: IsPrefixOf "a" "a" :~: 'True),
    toDyn (Refl :: IsPrefixOf "a" "abc" :~: 'True),
    toDyn (Refl :: IsPrefixOf "a" "abcdef" :~: 'True),
    --
    toDyn (Refl :: IsPrefixOf "abc" "" :~: 'False),
    toDyn (Refl :: IsPrefixOf "abc" "a" :~: 'False),
    toDyn (Refl :: IsPrefixOf "abc" "abc" :~: 'True),
    toDyn (Refl :: IsPrefixOf "abc" "abcdef" :~: 'True),
    --
    toDyn (Refl :: IsPrefixOf "abcdef" "" :~: 'False),
    toDyn (Refl :: IsPrefixOf "abcdef" "a" :~: 'False),
    toDyn (Refl :: IsPrefixOf "abcdef" "abc" :~: 'False),
    toDyn (Refl :: IsPrefixOf "abcdef" "abcdef" :~: 'True),
    --
    -- Insensitive tests
    --
    toDyn (Refl :: IsPrefixOfInsensitive "a" "" :~: 'False),
    toDyn (Refl :: IsPrefixOfInsensitive "a" "A" :~: 'True),
    toDyn (Refl :: IsPrefixOfInsensitive "a" "ABC" :~: 'True),
    toDyn (Refl :: IsPrefixOfInsensitive "a" "ABCDEF" :~: 'True),
    --
    toDyn (Refl :: IsPrefixOfInsensitive "abc" "" :~: 'False),
    toDyn (Refl :: IsPrefixOfInsensitive "abc" "A" :~: 'False),
    toDyn (Refl :: IsPrefixOfInsensitive "abc" "ABC" :~: 'True),
    toDyn (Refl :: IsPrefixOfInsensitive "abc" "ABCDEF" :~: 'True)
  ]
