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
import VCard.Util (Truth)
import VCard.XName
  ( XNameLowerSymbol,
    XNameSymbol,
    XNameUpperSymbol,
  )

shouldTypecheck :: (c :: Constraint) :~: Truth -> Assertion
shouldTypecheck r = deepseq r (pure ())

test_XNameSymbol :: TestTree
test_XNameSymbol =
  testCase "XName" $ do
    shouldTypecheck (Refl :: XNameSymbol "x-a" :~: Truth)
    shouldTypecheck (Refl :: XNameSymbol "X-a" :~: Truth)
    shouldTypecheck (Refl :: XNameSymbol "x-A" :~: Truth)
    shouldTypecheck (Refl :: XNameSymbol "X-A" :~: Truth)
    shouldTypecheck (Refl :: XNameSymbol "x-foo" :~: Truth)
    shouldTypecheck (Refl :: XNameSymbol "X-foo" :~: Truth)
    shouldTypecheck (Refl :: XNameSymbol "x-Foo" :~: Truth)
    shouldTypecheck (Refl :: XNameSymbol "X-Foo" :~: Truth)
    shouldTypecheck (Refl :: XNameSymbol "x-FOO" :~: Truth)
    shouldTypecheck (Refl :: XNameSymbol "X-FOO" :~: Truth)
    --
    shouldNotTypecheck (Refl :: XNameSymbol "" :~: Truth)
    shouldNotTypecheck (Refl :: XNameSymbol "a" :~: Truth)
    shouldNotTypecheck (Refl :: XNameSymbol "A" :~: Truth)
    shouldNotTypecheck (Refl :: XNameSymbol "foo" :~: Truth)
    shouldNotTypecheck (Refl :: XNameSymbol "Foo" :~: Truth)
    shouldNotTypecheck (Refl :: XNameSymbol "FOO" :~: Truth)
    shouldNotTypecheck (Refl :: XNameSymbol "x" :~: Truth)
    shouldNotTypecheck (Refl :: XNameSymbol "X" :~: Truth)
    shouldNotTypecheck (Refl :: XNameSymbol "x-" :~: Truth)
    shouldNotTypecheck (Refl :: XNameSymbol "X-" :~: Truth)

test_XNameLowerSymbol :: TestTree
test_XNameLowerSymbol =
  testCase "XNameLowerSymbol" $ do
    shouldTypecheck (Refl :: XNameLowerSymbol "x-a" :~: Truth)
    shouldTypecheck (Refl :: XNameLowerSymbol "x-foo" :~: Truth)
    --
    shouldNotTypecheck (Refl :: XNameLowerSymbol "x-Foo" :~: Truth)
    shouldNotTypecheck (Refl :: XNameLowerSymbol "X-Foo" :~: Truth)
    shouldNotTypecheck (Refl :: XNameLowerSymbol "x-FOO" :~: Truth)
    shouldNotTypecheck (Refl :: XNameLowerSymbol "X-FOO" :~: Truth)
    shouldNotTypecheck (Refl :: XNameLowerSymbol "x-A" :~: Truth)
    shouldNotTypecheck (Refl :: XNameLowerSymbol "X-a" :~: Truth)
    shouldNotTypecheck (Refl :: XNameLowerSymbol "X-A" :~: Truth)
    shouldNotTypecheck (Refl :: XNameLowerSymbol "X-foo" :~: Truth)
    --
    shouldNotTypecheck (Refl :: XNameLowerSymbol "" :~: Truth)
    shouldNotTypecheck (Refl :: XNameLowerSymbol "a" :~: Truth)
    shouldNotTypecheck (Refl :: XNameLowerSymbol "A" :~: Truth)
    shouldNotTypecheck (Refl :: XNameLowerSymbol "foo" :~: Truth)
    shouldNotTypecheck (Refl :: XNameLowerSymbol "Foo" :~: Truth)
    shouldNotTypecheck (Refl :: XNameLowerSymbol "FOO" :~: Truth)
    shouldNotTypecheck (Refl :: XNameLowerSymbol "x" :~: Truth)
    shouldNotTypecheck (Refl :: XNameLowerSymbol "X" :~: Truth)
    shouldNotTypecheck (Refl :: XNameLowerSymbol "x-" :~: Truth)
    shouldNotTypecheck (Refl :: XNameLowerSymbol "X-" :~: Truth)

test_XNameUpperSymbol :: TestTree
test_XNameUpperSymbol =
  testCase "XNameUpperSymbol" $ do
    shouldTypecheck (Refl :: XNameUpperSymbol "X-A" :~: Truth)
    shouldTypecheck (Refl :: XNameUpperSymbol "X-FOO" :~: Truth)
    --
    shouldNotTypecheck (Refl :: XNameUpperSymbol "x-FOO" :~: Truth)
    shouldNotTypecheck (Refl :: XNameUpperSymbol "x-foo" :~: Truth)
    shouldNotTypecheck (Refl :: XNameUpperSymbol "X-foo" :~: Truth)
    shouldNotTypecheck (Refl :: XNameUpperSymbol "x-Foo" :~: Truth)
    shouldNotTypecheck (Refl :: XNameUpperSymbol "X-Foo" :~: Truth)
    shouldNotTypecheck (Refl :: XNameUpperSymbol "x-a" :~: Truth)
    shouldNotTypecheck (Refl :: XNameUpperSymbol "X-a" :~: Truth)
    shouldNotTypecheck (Refl :: XNameUpperSymbol "x-A" :~: Truth)
    --
    shouldNotTypecheck (Refl :: XNameUpperSymbol "" :~: Truth)
    shouldNotTypecheck (Refl :: XNameUpperSymbol "a" :~: Truth)
    shouldNotTypecheck (Refl :: XNameUpperSymbol "A" :~: Truth)
    shouldNotTypecheck (Refl :: XNameUpperSymbol "foo" :~: Truth)
    shouldNotTypecheck (Refl :: XNameUpperSymbol "Foo" :~: Truth)
    shouldNotTypecheck (Refl :: XNameUpperSymbol "FOO" :~: Truth)
    shouldNotTypecheck (Refl :: XNameUpperSymbol "x" :~: Truth)
    shouldNotTypecheck (Refl :: XNameUpperSymbol "X" :~: Truth)
    shouldNotTypecheck (Refl :: XNameUpperSymbol "x-" :~: Truth)
    shouldNotTypecheck (Refl :: XNameUpperSymbol "X-" :~: Truth)
