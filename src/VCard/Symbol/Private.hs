-- SPDX-FileCopyrightText: Copyright Preetham Gujjula
-- SPDX-License-Identifier: BSD-3-Clause

module VCard.Symbol.Private
  ( -- * 'Char' singleton
    SChar,
    charSing,
    withKnownChar,
    withSomeSChar,

    -- * 'Symbol' singleton
    SSymbol,
    symbolSing,
    withKnownSymbol,
    withSomeSSymbol,

    -- * Singleton equality
    testSCharEquality,
    testSSymbolEquality,

    -- * Change case of 'Symbol'

    -- ** Lower case
    ToLower,
    sToLower,

    -- ** Upper case
    ToUpper,
    sToUpper,

    -- * Prefix checking
    IsPrefixOf,
    sIsPrefixOf,
    IsPrefixOfInsensitive,
    sIsPrefixOfInsensitive,

    -- * Cons and uncons
    ConsSymbol,
    sConsSymbol,
    UnconsSymbol,
    sUnconsSymbol,

    -- * Miscellaneous utilities
    ToList,
    sToList,
    FromList,
    sFromList,
    Take,
    sTake,
    Drop,
    sDrop,
    Length,
    sLength,
  )
where

import GHC.TypeLits (ConsSymbol, UnconsSymbol)
import VCard.Symbol.Private.Case (ToLower, ToUpper, sToLower, sToUpper)
import VCard.Symbol.Private.Compat
  ( SChar,
    SSymbol,
    charSing,
    sConsSymbol,
    sUnconsSymbol,
    symbolSing,
    testSCharEquality,
    testSSymbolEquality,
    withKnownChar,
    withKnownSymbol,
    withSomeSChar,
    withSomeSSymbol,
  )
import VCard.Symbol.Private.Length (Length, sLength)
import VCard.Symbol.Private.List (FromList, ToList, sFromList, sToList)
import VCard.Symbol.Private.Prefix
  ( IsPrefixOf,
    IsPrefixOfInsensitive,
    sIsPrefixOf,
    sIsPrefixOfInsensitive,
  )
import VCard.Symbol.Private.Slice (Drop, Take, sDrop, sTake)
