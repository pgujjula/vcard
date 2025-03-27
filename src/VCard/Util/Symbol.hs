-- SPDX-FileCopyrightText: Copyright Preetham Gujjula
-- SPDX-License-Identifier: BSD-3-Clause

module VCard.Util.Symbol
  ( -- * 'Char' singleton
    SChar,
    charSing,
    withKnownChar,
    withSomeSChar,
    fromSChar,

    -- * 'GHC.TypeLits.Symbol' singleton
    SSymbol,
    symbolSing,
    withKnownSymbol,
    withSomeSSymbol,
    fromSSymbol,

    -- * Singleton equality
    testSCharEquality,
    testSSymbolEquality,

    -- * Change case of 'GHC.TypeLits.Symbol'

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

    -- * Cons and Snoc
    ConsSymbol,
    sConsSymbol,
    UnconsSymbol,
    sUnconsSymbol,
    SnocSymbol,
    sSnocSymbol,
    UnsnocSymbol,
    sUnsnocSymbol,

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
import VCard.Util.Symbol.Private.Case (ToLower, ToUpper, sToLower, sToUpper)
import VCard.Util.Symbol.Private.Compat
  ( SChar,
    SSymbol,
    charSing,
    fromSChar,
    fromSSymbol,
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
import VCard.Util.Symbol.Private.Length (Length, sLength)
import VCard.Util.Symbol.Private.List (FromList, ToList, sFromList, sToList)
import VCard.Util.Symbol.Private.Parse ()
import VCard.Util.Symbol.Private.Prefix
  ( IsPrefixOf,
    IsPrefixOfInsensitive,
    sIsPrefixOf,
    sIsPrefixOfInsensitive,
  )
import VCard.Util.Symbol.Private.Serialize ()
import VCard.Util.Symbol.Private.Slice (Drop, Take, sDrop, sTake)
import VCard.Util.Symbol.Private.Snoc
  ( SnocSymbol,
    UnsnocSymbol,
    sSnocSymbol,
    sUnsnocSymbol,
  )
