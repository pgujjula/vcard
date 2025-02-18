{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

module VCard.Symbol.Private.Compat.Old
  ( charSing,
    symbolSing,
    testSCharEquality,
    testSSymbolEquality,
    sConsSymbol,
    sUnconsSymbol,
  )
where

import Data.Maybe.Singletons (SMaybe (SJust, SNothing))
import Data.Proxy (Proxy (..))
import Data.Tuple.Singletons (STuple2 (..))
import Data.Type.Equality (testEquality, (:~:))
import GHC.TypeLits
  ( KnownChar,
    KnownSymbol,
    SomeChar (..),
    SomeSymbol (..),
    UnconsSymbol,
    someCharVal,
    someSymbolVal,
    symbolVal,
  )
import GHC.TypeLits.Singletons
  ( SChar (..),
    SSymbol (SSym),
    sConsSymbol,
    withKnownChar,
    withKnownSymbol,
  )
import Type.Reflection (TypeRep, typeRep)
import Unsafe.Coerce (unsafeCoerce)

charSing :: (KnownChar c) => SChar c
charSing = SChar

symbolSing :: (KnownSymbol s) => SSymbol s
symbolSing = SSym

testSCharEquality :: forall a b. SChar a -> SChar b -> Maybe (a :~: b)
testSCharEquality sa sb =
  withKnownChar sa $
    withKnownChar sb $
      testEquality (typeRep :: TypeRep a) (typeRep :: TypeRep b)

testSSymbolEquality :: forall a b. SSymbol a -> SSymbol b -> Maybe (a :~: b)
testSSymbolEquality sa sb =
  withKnownSymbol sa $
    withKnownSymbol sb $
      testEquality (typeRep :: TypeRep a) (typeRep :: TypeRep b)

-- There is a bug in `sUnconsSymbol` in the singletons-base library for GHC 9.2
-- and 9.4, so we implement our own
sUnconsSymbol :: SSymbol s -> SMaybe (UnconsSymbol s)
sUnconsSymbol ss =
  case fromSSymbol ss of
    [] -> unsafeCoerce SNothing
    (c : s') ->
      withSomeSChar c $ \sc ->
        withSomeSSymbol s' $ \ss' ->
          unsafeCoerce (SJust (STuple2 sc ss'))

fromSSymbol :: forall s. SSymbol s -> String
fromSSymbol ss = withKnownSymbol ss (symbolVal (symbolSing :: SSymbol s))

withSomeSChar :: Char -> (forall c. SChar c -> r) -> r
withSomeSChar c f =
  case someCharVal c of
    SomeChar (Proxy :: Proxy c) -> f (charSing :: SChar c)

withSomeSSymbol :: String -> (forall s. SSymbol s -> r) -> r
withSomeSSymbol c f =
  case someSymbolVal c of
    SomeSymbol (Proxy :: Proxy c) -> f (symbolSing :: SSymbol c)
