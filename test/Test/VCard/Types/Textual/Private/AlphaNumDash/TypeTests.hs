-- SPDX-FileCopyrightText: Copyright Preetham Gujjula
-- SPDX-License-Identifier: BSD-3-Clause
{-# OPTIONS_GHC -Wno-deferred-type-errors #-}
{-# OPTIONS_GHC -fdefer-type-errors #-}

-- We define the type tests in a separate module so we can enable
-- -fdefer-type-errors for just these tests, without affecting the singleton
-- tests.
module Test.VCard.Types.Textual.Private.AlphaNumDash.TypeTests
  ( test_AlphaNumDashSymbol,
    test_AlphaNumDashLowerSymbol,
    test_AlphaNumDashUpperSymbol,
  )
where

import Control.DeepSeq (deepseq)
import Data.Kind (Constraint)
import Data.Type.Equality ((:~:) (Refl))
import Test.ShouldNotTypecheck (shouldNotTypecheck)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (Assertion, testCase)
import Test.Util (Truth)
import VCard.Types.Textual.Private.AlphaNumDash
  ( AlphaNumDashLowerSymbol,
    AlphaNumDashSymbol,
    AlphaNumDashUpperSymbol,
  )

shouldTypecheck :: (c :: Constraint) :~: Truth -> Assertion
shouldTypecheck r = deepseq r (pure ())

test_AlphaNumDashSymbol :: TestTree
test_AlphaNumDashSymbol =
  testCase "AlphaNumDashSymbol" $ do
    shouldTypecheck (Refl :: AlphaNumDashSymbol "a" :~: Truth)
    shouldTypecheck (Refl :: AlphaNumDashSymbol "foo-BAR-123" :~: Truth)
    shouldTypecheck
      ( Refl ::
          AlphaNumDashSymbol
            "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789-"
            :~: Truth
      )

    --
    shouldNotTypecheck (Refl :: AlphaNumDashSymbol "" :~: Truth)
    shouldNotTypecheck (Refl :: AlphaNumDashSymbol "foo-BAR-!23" :~: Truth)

test_AlphaNumDashLowerSymbol :: TestTree
test_AlphaNumDashLowerSymbol =
  testCase "AlphaNumDashLowerSymbol" $ do
    shouldTypecheck (Refl :: AlphaNumDashLowerSymbol "a" :~: Truth)
    shouldTypecheck (Refl :: AlphaNumDashLowerSymbol "foo-bar-123" :~: Truth)
    shouldTypecheck
      ( Refl ::
          AlphaNumDashLowerSymbol
            "abcdefghijklmnopqrstuvwxyz0123456789-"
            :~: Truth
      )

    --
    shouldNotTypecheck (Refl :: AlphaNumDashLowerSymbol "" :~: Truth)
    shouldNotTypecheck
      (Refl :: AlphaNumDashLowerSymbol "foo-bAr-123" :~: Truth)
    shouldNotTypecheck
      (Refl :: AlphaNumDashLowerSymbol "foo-bar-!23" :~: Truth)

test_AlphaNumDashUpperSymbol :: TestTree
test_AlphaNumDashUpperSymbol =
  testGroup
    "AlphaNumDashUpperSymbol"
    [ test_AlphaNumDashUpperSymbol_valid,
      test_AlphaNumDashUpperSymbol_invalid
    ]

test_AlphaNumDashUpperSymbol_valid :: TestTree
test_AlphaNumDashUpperSymbol_valid =
  testCase "valid" $ do
    shouldTypecheck (Refl :: AlphaNumDashUpperSymbol "A" :~: Truth)
    shouldTypecheck (Refl :: AlphaNumDashUpperSymbol "FOO-BAR-123" :~: Truth)
    shouldTypecheck
      ( Refl ::
          AlphaNumDashUpperSymbol
            "ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789-"
            :~: Truth
      )

-- shouldNotTypecheck is very fragile and potentially buggy. For example, with
-- GHC 9.4.8, changing "GOO-bAR-123" to "FOO-bAR-123" below causes
-- shouldNotTypecheck to throw a runtime error saying "Make sure the expression
-- has an NFData instance!"
--
-- Also, consolidating test_AlphaNumDashUpperSymbol_valid and
-- test_AlphaNumDashUpperSymbol_invalid into a single test causes the same
-- error.
test_AlphaNumDashUpperSymbol_invalid :: TestTree
test_AlphaNumDashUpperSymbol_invalid =
  testCase "invalid" $ do
    shouldNotTypecheck (Refl :: AlphaNumDashUpperSymbol "" :~: Truth)
    shouldNotTypecheck
      (Refl :: AlphaNumDashUpperSymbol "GOO-bAR-123" :~: Truth)
    shouldNotTypecheck
      (Refl :: AlphaNumDashUpperSymbol "GOO-BAR-!23" :~: Truth)
