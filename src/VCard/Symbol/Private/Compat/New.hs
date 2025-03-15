-- SPDX-FileCopyrightText: Copyright Preetham Gujjula
-- SPDX-License-Identifier: BSD-3-Clause

module VCard.Symbol.Private.Compat.New
  ( SChar,
    SSymbol,
    charSing,
    symbolSing,
    testSCharEquality,
    testSSymbolEquality,
    withKnownChar,
    withKnownSymbol,
    withSomeSChar,
    withSomeSSymbol,
    fromSChar,
    fromSSymbol,
    sConsSymbol,
    sUnconsSymbol,
  )
where

import Data.Maybe.Singletons (SMaybe)
import Data.Type.Equality (testEquality, (:~:))
import GHC.TypeLits
  ( ConsSymbol,
    KnownChar,
    KnownSymbol,
    SChar,
    SSymbol,
    UnconsSymbol,
    charSing,
    fromSChar,
    fromSSymbol,
    symbolSing,
    withSomeSChar,
    withSomeSSymbol,
  )
import GHC.TypeLits qualified as GHC (withKnownChar, withKnownSymbol)
import GHC.TypeLits.Singletons qualified as Singletons

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

-- | Singleton of 'ConsSymbol'.
sConsSymbol :: SChar c -> SSymbol s -> SSymbol (ConsSymbol c s)
sConsSymbol = Singletons.sConsSymbol

-- | Singleton of 'UnconsSymbol'.
sUnconsSymbol :: SSymbol s -> SMaybe (UnconsSymbol s)
sUnconsSymbol = Singletons.sUnconsSymbol
