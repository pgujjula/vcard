-- SPDX-FileCopyrightText: Copyright Preetham Gujjula
-- SPDX-License-Identifier: BSD-3-Clause
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -Wno-deferred-type-errors #-}
{-# OPTIONS_GHC -fdefer-type-errors #-}

-- We define the type tests in a separate module so we can enable
-- -fdefer-type-errors for just these tests, without affecting the singleton
-- tests.
module Test.VCard.XName.TypeTests
  ( test_XNameSymbol,
    test_XNameLowerSymbol,
    test_XNameUpperSymbol,
  )
where

import Control.DeepSeq (deepseq)
import Data.Kind (Constraint)
import Data.Type.Equality ((:~:) (Refl))
import Test.ShouldNotTypecheck (shouldNotTypecheck)
import Test.Tasty (TestTree)
import Test.Tasty.HUnit (Assertion, testCase)
import VCard.XName
  ( XNameLowerSymbol,
    XNameSymbol,
    XNameUpperSymbol,
  )

type Valid :: Constraint
type Valid = ()

shouldTypecheck :: (c :: Constraint) :~: Valid -> Assertion
shouldTypecheck r = deepseq r (pure ())

test_XNameSymbol :: TestTree
test_XNameSymbol =
  testCase "XName" $ do
    shouldTypecheck (Refl :: XNameSymbol "x-a" :~: Valid)
    shouldTypecheck (Refl :: XNameSymbol "X-a" :~: Valid)
    shouldTypecheck (Refl :: XNameSymbol "x-A" :~: Valid)
    shouldTypecheck (Refl :: XNameSymbol "X-A" :~: Valid)
    shouldTypecheck (Refl :: XNameSymbol "x-foo" :~: Valid)
    shouldTypecheck (Refl :: XNameSymbol "X-foo" :~: Valid)
    shouldTypecheck (Refl :: XNameSymbol "x-Foo" :~: Valid)
    shouldTypecheck (Refl :: XNameSymbol "X-Foo" :~: Valid)
    shouldTypecheck (Refl :: XNameSymbol "x-FOO" :~: Valid)
    shouldTypecheck (Refl :: XNameSymbol "X-FOO" :~: Valid)
    --
    shouldNotTypecheck (Refl :: XNameSymbol "" :~: Valid)
    shouldNotTypecheck (Refl :: XNameSymbol "a" :~: Valid)
    shouldNotTypecheck (Refl :: XNameSymbol "A" :~: Valid)
    shouldNotTypecheck (Refl :: XNameSymbol "foo" :~: Valid)
    shouldNotTypecheck (Refl :: XNameSymbol "Foo" :~: Valid)
    shouldNotTypecheck (Refl :: XNameSymbol "FOO" :~: Valid)
    shouldNotTypecheck (Refl :: XNameSymbol "x" :~: Valid)
    shouldNotTypecheck (Refl :: XNameSymbol "X" :~: Valid)
    shouldNotTypecheck (Refl :: XNameSymbol "x-" :~: Valid)
    shouldNotTypecheck (Refl :: XNameSymbol "X-" :~: Valid)

test_XNameLowerSymbol :: TestTree
test_XNameLowerSymbol =
  testCase "XNameLowerSymbol" $ do
    shouldTypecheck (Refl :: XNameLowerSymbol "x-a" :~: Valid)
    shouldTypecheck (Refl :: XNameLowerSymbol "x-foo" :~: Valid)
    --
    shouldNotTypecheck (Refl :: XNameLowerSymbol "x-Foo" :~: Valid)
    shouldNotTypecheck (Refl :: XNameLowerSymbol "X-Foo" :~: Valid)
    shouldNotTypecheck (Refl :: XNameLowerSymbol "x-FOO" :~: Valid)
    shouldNotTypecheck (Refl :: XNameLowerSymbol "X-FOO" :~: Valid)
    shouldNotTypecheck (Refl :: XNameLowerSymbol "x-A" :~: Valid)
    shouldNotTypecheck (Refl :: XNameLowerSymbol "X-a" :~: Valid)
    shouldNotTypecheck (Refl :: XNameLowerSymbol "X-A" :~: Valid)
    shouldNotTypecheck (Refl :: XNameLowerSymbol "X-foo" :~: Valid)
    --
    shouldNotTypecheck (Refl :: XNameLowerSymbol "" :~: Valid)
    shouldNotTypecheck (Refl :: XNameLowerSymbol "a" :~: Valid)
    shouldNotTypecheck (Refl :: XNameLowerSymbol "A" :~: Valid)
    shouldNotTypecheck (Refl :: XNameLowerSymbol "foo" :~: Valid)
    shouldNotTypecheck (Refl :: XNameLowerSymbol "Foo" :~: Valid)
    shouldNotTypecheck (Refl :: XNameLowerSymbol "FOO" :~: Valid)
    shouldNotTypecheck (Refl :: XNameLowerSymbol "x" :~: Valid)
    shouldNotTypecheck (Refl :: XNameLowerSymbol "X" :~: Valid)
    shouldNotTypecheck (Refl :: XNameLowerSymbol "x-" :~: Valid)
    shouldNotTypecheck (Refl :: XNameLowerSymbol "X-" :~: Valid)

test_XNameUpperSymbol :: TestTree
test_XNameUpperSymbol =
  testCase "XNameUpperSymbol" $ do
    shouldTypecheck (Refl :: XNameUpperSymbol "X-A" :~: Valid)
    shouldTypecheck (Refl :: XNameUpperSymbol "X-FOO" :~: Valid)
    --
    shouldNotTypecheck (Refl :: XNameUpperSymbol "x-FOO" :~: Valid)
    shouldNotTypecheck (Refl :: XNameUpperSymbol "x-foo" :~: Valid)
    shouldNotTypecheck (Refl :: XNameUpperSymbol "X-foo" :~: Valid)
    shouldNotTypecheck (Refl :: XNameUpperSymbol "x-Foo" :~: Valid)
    shouldNotTypecheck (Refl :: XNameUpperSymbol "X-Foo" :~: Valid)
    shouldNotTypecheck (Refl :: XNameUpperSymbol "x-a" :~: Valid)
    shouldNotTypecheck (Refl :: XNameUpperSymbol "X-a" :~: Valid)
    shouldNotTypecheck (Refl :: XNameUpperSymbol "x-A" :~: Valid)
    --
    shouldNotTypecheck (Refl :: XNameUpperSymbol "" :~: Valid)
    shouldNotTypecheck (Refl :: XNameUpperSymbol "a" :~: Valid)
    shouldNotTypecheck (Refl :: XNameUpperSymbol "A" :~: Valid)
    shouldNotTypecheck (Refl :: XNameUpperSymbol "foo" :~: Valid)
    shouldNotTypecheck (Refl :: XNameUpperSymbol "Foo" :~: Valid)
    shouldNotTypecheck (Refl :: XNameUpperSymbol "FOO" :~: Valid)
    shouldNotTypecheck (Refl :: XNameUpperSymbol "x" :~: Valid)
    shouldNotTypecheck (Refl :: XNameUpperSymbol "X" :~: Valid)
    shouldNotTypecheck (Refl :: XNameUpperSymbol "x-" :~: Valid)
    shouldNotTypecheck (Refl :: XNameUpperSymbol "X-" :~: Valid)
