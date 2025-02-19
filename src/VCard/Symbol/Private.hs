-- SPDX-FileCopyrightText: Copyright Preetham Gujjula
-- SPDX-License-Identifier: BSD-3-Clause

module VCard.Symbol.Private
  ( -- * 'Char' singleton
    SChar,
    charSing,

    -- * 'Symbol' singleton
    SSymbol,
    symbolSing,

    -- * Singleton equality
    testSCharEquality,
    testSSymbolEquality,
  )
where

import VCard.Symbol.Private.Compat
  ( SChar,
    SSymbol,
    charSing,
    symbolSing,
    testSCharEquality,
    testSSymbolEquality,
  )
