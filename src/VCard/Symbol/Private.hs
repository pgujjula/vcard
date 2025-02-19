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

    -- * Change case of 'Char'

    -- ** Lower case
    ToLowerChar,
    sToLowerChar,

    -- ** Upper case
    ToUpperChar,
    sToUpperChar,
  )
where

import VCard.Symbol.Private.Case
  ( ToLowerChar,
    ToUpperChar,
    sToLowerChar,
    sToUpperChar,
  )
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
