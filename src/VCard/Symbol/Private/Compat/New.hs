-- SPDX-FileCopyrightText: Copyright Preetham Gujjula
-- SPDX-License-Identifier: BSD-3-Clause
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE TypeOperators #-}

module VCard.Symbol.Private.Compat.New
  ( SChar,
    SSymbol,
    charSing,
    symbolSing,
    testSCharEquality,
    testSSymbolEquality,
    withKnownChar,
    withKnownSymbol,
  )
where

import Data.Type.Equality (testEquality, (:~:))
import GHC.TypeLits
  ( KnownChar,
    KnownSymbol,
    SChar,
    SSymbol,
    charSing,
    symbolSing,
  )
import GHC.TypeLits qualified as GHC (withKnownChar, withKnownSymbol)

-- | Conditionally prove the equality of two 'SChar's.
testSCharEquality :: SChar a -> SChar b -> Maybe (a :~: b)
testSCharEquality = testEquality

-- | Conditionally prove the equality of two 'SSymbol's.
testSSymbolEquality :: SSymbol a -> SSymbol b -> Maybe (a :~: b)
testSSymbolEquality = testEquality

-- | Obtain a @'KnownChar' c@ constraint given an @'SChar' c@ value.
withKnownChar :: SChar c -> ((KnownChar c) => r) -> r
withKnownChar = GHC.withKnownChar

-- | Obtain a @'KnownSymbol' s@ constraint given an @'SSymbol' s@ value.
withKnownSymbol :: SSymbol s -> ((KnownSymbol s) => r) -> r
withKnownSymbol = GHC.withKnownSymbol
