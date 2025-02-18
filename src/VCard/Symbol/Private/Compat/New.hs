{-# LANGUAGE TypeOperators #-}

module VCard.Symbol.Private.Compat.New
  ( charSing,
    symbolSing,
    testSCharEquality,
    testSSymbolEquality,
    sConsSymbol,
    sUnconsSymbol,
  )
where

import Data.Type.Equality (testEquality, (:~:))
import GHC.TypeLits (charSing, symbolSing)
import GHC.TypeLits.Singletons (SChar, SSymbol, sConsSymbol, sUnconsSymbol)

testSCharEquality :: SChar a -> SChar b -> Maybe (a :~: b)
testSCharEquality = testEquality

testSSymbolEquality :: SSymbol a -> SSymbol b -> Maybe (a :~: b)
testSSymbolEquality = testEquality
