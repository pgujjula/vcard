-- SPDX-FileCopyrightText: Copyright Preetham Gujjula
-- SPDX-License-Identifier: BSD-3-Clause

module VCard.Symbol.Private
  ( -- * 'Char' and 'Symbol' singletons
    charSing,
    symbolSing,

    -- * Singleton equality
    testSCharEquality,
    testSSymbolEquality,
  )
where

import VCard.Symbol.Private.Compat
  ( charSing,
    symbolSing,
    testSCharEquality,
    testSSymbolEquality,
  )
