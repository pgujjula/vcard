-- SPDX-FileCopyrightText: Copyright Preetham Gujjula
-- SPDX-License-Identifier: BSD-3-Clause

module VCard.Symbol.Private
  ( -- * 'Char' singleton
    SChar,
    charSing,
    withKnownChar,

    -- * 'Symbol' singleton
    SSymbol,
    symbolSing,
    withKnownSymbol,

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

    -- * Miscellaneous utilities
    ToList,
    sToList,
    Length,
    sLength,
  )
where

import VCard.Symbol.Private.Case (ToLower, ToUpper, sToLower, sToUpper)
import VCard.Symbol.Private.Compat
  ( SChar,
    SSymbol,
    charSing,
    symbolSing,
    testSCharEquality,
    testSSymbolEquality,
    withKnownChar,
    withKnownSymbol,
  )
import VCard.Symbol.Private.Length (Length, sLength)
import VCard.Symbol.Private.List (ToList, sToList)
import VCard.Symbol.Private.Prefix
  ( IsPrefixOf,
    IsPrefixOfInsensitive,
    sIsPrefixOf,
    sIsPrefixOfInsensitive,
  )
