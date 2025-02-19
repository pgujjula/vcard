-- SPDX-FileCopyrightText: Copyright Preetham Gujjula
-- SPDX-License-Identifier: BSD-3-Clause
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

module VCard.Symbol.Private.Compat.Old
  ( charSing,
    symbolSing,
    testSCharEquality,
    testSSymbolEquality,
  )
where

import Data.Type.Equality (testEquality, (:~:))
import GHC.TypeLits (KnownChar, KnownSymbol)
import GHC.TypeLits.Singletons
  ( SChar (..),
    SSymbol (SSym),
    withKnownChar,
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
