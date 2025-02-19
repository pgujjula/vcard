-- SPDX-FileCopyrightText: Copyright Preetham Gujjula
-- SPDX-License-Identifier: BSD-3-Clause
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

module VCard.Symbol.Private.Compat.Old
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
import GHC.TypeLits (KnownChar, KnownSymbol)
import GHC.TypeLits.Singletons
  ( SChar (..),
    SSymbol (SSym),
  )
import GHC.TypeLits.Singletons qualified as Singletons
  ( withKnownChar,
    withKnownSymbol,
  )
import Type.Reflection (TypeRep, typeRep)

charSing :: (KnownChar c) => SChar c
charSing = SChar

symbolSing :: (KnownSymbol s) => SSymbol s
symbolSing = SSym

-- | Conditionally prove the equality of two 'SChar's.
testSCharEquality :: SChar a -> SChar b -> Maybe (a :~: b)
testSCharEquality = testSCharEquality_

testSCharEquality_ :: forall a b. SChar a -> SChar b -> Maybe (a :~: b)
testSCharEquality_ sa sb =
  withKnownChar sa $
    withKnownChar sb $
      testEquality (typeRep :: TypeRep a) (typeRep :: TypeRep b)

-- | Conditionally prove the equality of two 'SSymbol's.
testSSymbolEquality :: SSymbol a -> SSymbol b -> Maybe (a :~: b)
testSSymbolEquality = testSSymbolEquality_

testSSymbolEquality_ :: forall a b. SSymbol a -> SSymbol b -> Maybe (a :~: b)
testSSymbolEquality_ sa sb =
  withKnownSymbol sa $
    withKnownSymbol sb $
      testEquality (typeRep :: TypeRep a) (typeRep :: TypeRep b)

-- | Obtain a @'KnownChar' c@ constraint given an @'SChar' c@ value.
withKnownChar :: SChar c -> ((KnownChar c) => r) -> r
withKnownChar = Singletons.withKnownChar

-- | Obtain a @'KnownSymbol' s@ constraint given an @'SSymbol' s@ value.
withKnownSymbol :: SSymbol s -> ((KnownSymbol s) => r) -> r
withKnownSymbol = Singletons.withKnownSymbol
