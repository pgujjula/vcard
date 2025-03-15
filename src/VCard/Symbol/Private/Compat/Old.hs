-- SPDX-FileCopyrightText: Copyright Preetham Gujjula
-- SPDX-License-Identifier: BSD-3-Clause

module VCard.Symbol.Private.Compat.Old
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

import Data.Maybe.Singletons (SMaybe (..))
import Data.Proxy (Proxy (..))
import Data.Tuple.Singletons (STuple2 (..))
import Data.Type.Equality (testEquality, (:~:))
import GHC.TypeLits
  ( ConsSymbol,
    KnownChar,
    KnownSymbol,
    SomeChar (..),
    SomeSymbol (..),
    Symbol,
    UnconsSymbol,
    charVal,
    someCharVal,
    someSymbolVal,
    symbolVal,
  )
import GHC.TypeLits.Singletons (SChar (..), SSymbol (SSym))
import GHC.TypeLits.Singletons qualified as Singletons
  ( withKnownChar,
    withKnownSymbol,
  )
import Type.Reflection (TypeRep, typeRep)
import Unsafe.Coerce (unsafeCoerce)

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

-- | Convert a 'Char' into an 'SChar c' value.
withSomeSChar :: Char -> (forall (c :: Char). SChar c -> r) -> r
withSomeSChar char f =
  case someCharVal char of
    SomeChar (Proxy :: Proxy c) -> f (charSing :: SChar c)

-- | Convert a 'String' into an 'SSymbol s' value.
withSomeSSymbol :: String -> (forall (s :: Symbol). SSymbol s -> r) -> r
withSomeSSymbol string f =
  case someSymbolVal string of
    SomeSymbol (Proxy :: Proxy s) -> f (symbolSing :: SSymbol s)

-- | Return the 'Char' corresponding to @c@ in an @'SChar' c@ value.
fromSChar :: SChar s -> Char
fromSChar sc = withKnownChar sc (charVal sc)

-- | Return the 'String' corresponding to @s@ in an @'SSymbol' s@ value.
fromSSymbol :: SSymbol s -> String
fromSSymbol ss = withKnownSymbol ss (symbolVal ss)

-- | Singleton of 'ConsSymbol'.

-- See the note below about the bug in `sUnconsSymbol` from `singleton-base`.
-- Out of an abundance of caution, we implement a custom sConsSymbol here as
-- well.
sConsSymbol :: forall c s. SChar c -> SSymbol s -> SSymbol (ConsSymbol c s)
sConsSymbol sc ss =
  let c = fromSChar sc
      s = fromSSymbol ss
   in case someSymbolVal (c : s) of
        SomeSymbol (Proxy :: Proxy s') ->
          unsafeCoerce (symbolSing :: SSymbol s')

-- | Singleton of 'UnconsSymbol'.

-- There is a bug in `sUnconsSymbol` in the `singletons-base` library when
-- GHC < 9.6, so we implement our own.
sUnconsSymbol :: forall s. SSymbol s -> SMaybe (UnconsSymbol s)
sUnconsSymbol ss =
  case fromSSymbol ss of
    [] -> unsafeCoerce SNothing
    (c : s') ->
      case someCharVal c of
        SomeChar (Proxy :: Proxy c) ->
          case someSymbolVal s' of
            SomeSymbol (Proxy :: Proxy s') ->
              let sc = charSing :: SChar c
                  ss' = symbolSing :: SSymbol s'
               in unsafeCoerce (SJust (STuple2 sc ss'))
