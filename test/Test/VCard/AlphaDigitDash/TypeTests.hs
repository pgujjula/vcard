-- SPDX-FileCopyrightText: Copyright Preetham Gujjula
-- SPDX-License-Identifier: BSD-3-Clause
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -Wno-deferred-type-errors #-}
{-# OPTIONS_GHC -fdefer-type-errors #-}

-- We define the type tests in a separate module so we can enable
-- -fdefer-type-errors for just these tests, without affecting the singleton
-- tests.
module Test.VCard.AlphaDigitDash.TypeTests
  ( test_AlphaDigitDashSymbol,
    test_AlphaDigitDashLowerSymbol,
    test_AlphaDigitDashUpperSymbol,
  )
where

import Control.DeepSeq (deepseq)
import Data.Kind (Constraint)
import Data.Type.Equality ((:~:) (Refl))
import Test.ShouldNotTypecheck (shouldNotTypecheck)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (Assertion, testCase)
import VCard.AlphaDigitDash
  ( AlphaDigitDashLowerSymbol,
    AlphaDigitDashSymbol,
    AlphaDigitDashUpperSymbol,
  )
import VCard.Util (Truth)

shouldTypecheck :: (c :: Constraint) :~: Truth -> Assertion
shouldTypecheck r = deepseq r (pure ())

test_AlphaDigitDashSymbol :: TestTree
test_AlphaDigitDashSymbol =
  testCase "AlphaDigitDashSymbol" $ do
    shouldTypecheck (Refl :: AlphaDigitDashSymbol "a" :~: Truth)
    shouldTypecheck (Refl :: AlphaDigitDashSymbol "foo-BAR-123" :~: Truth)
    shouldTypecheck
      ( Refl ::
          AlphaDigitDashSymbol
            "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789-"
            :~: Truth
      )

    --
    shouldNotTypecheck (Refl :: AlphaDigitDashSymbol "" :~: Truth)
    shouldNotTypecheck (Refl :: AlphaDigitDashSymbol "foo-BAR-!23" :~: Truth)

test_AlphaDigitDashLowerSymbol :: TestTree
test_AlphaDigitDashLowerSymbol =
  testCase "AlphaDigitDashLowerSymbol" $ do
    shouldTypecheck (Refl :: AlphaDigitDashLowerSymbol "a" :~: Truth)
    shouldTypecheck (Refl :: AlphaDigitDashLowerSymbol "foo-bar-123" :~: Truth)
    shouldTypecheck
      ( Refl ::
          AlphaDigitDashLowerSymbol
            "abcdefghijklmnopqrstuvwxyz0123456789-"
            :~: Truth
      )

    --
    shouldNotTypecheck (Refl :: AlphaDigitDashLowerSymbol "" :~: Truth)
    shouldNotTypecheck
      (Refl :: AlphaDigitDashLowerSymbol "foo-bAr-123" :~: Truth)
    shouldNotTypecheck
      (Refl :: AlphaDigitDashLowerSymbol "foo-bar-!23" :~: Truth)

test_AlphaDigitDashUpperSymbol :: TestTree
test_AlphaDigitDashUpperSymbol =
  testGroup
    "AlphaDigitDashUpperSymbol"
    [ test_AlphaDigitDashUpperSymbol_valid,
      test_AlphaDigitDashUpperSymbol_invalid
    ]

test_AlphaDigitDashUpperSymbol_valid :: TestTree
test_AlphaDigitDashUpperSymbol_valid =
  testCase "valid" $ do
    shouldTypecheck (Refl :: AlphaDigitDashUpperSymbol "A" :~: Truth)
    shouldTypecheck (Refl :: AlphaDigitDashUpperSymbol "FOO-BAR-123" :~: Truth)
    shouldTypecheck
      ( Refl ::
          AlphaDigitDashUpperSymbol
            "ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789-"
            :~: Truth
      )

-- shouldNotTypecheck is very fragile and potentially buggy. For example, with
-- GHC 9.4.8, changing "GOO-bAR-123" to "FOO-bAR-123" below causes
-- shouldNotTypecheck to throw a runtime error saying "Make sure the expression
-- has an NFData instance!"
--
-- Also, consolidating test_AlphaDigitDashUpperSymbol_valid and
-- test_AlphaDigitDashUpperSymbol_invalid into a single test causes the same
-- error.
test_AlphaDigitDashUpperSymbol_invalid :: TestTree
test_AlphaDigitDashUpperSymbol_invalid =
  testCase "invalid" $ do
    shouldNotTypecheck (Refl :: AlphaDigitDashUpperSymbol "" :~: Truth)
    shouldNotTypecheck
      (Refl :: AlphaDigitDashUpperSymbol "GOO-bAR-123" :~: Truth)
    shouldNotTypecheck
      (Refl :: AlphaDigitDashUpperSymbol "GOO-BAR-!23" :~: Truth)
