-- SPDX-FileCopyrightText: Copyright Preetham Gujjula
-- SPDX-License-Identifier: BSD-3-Clause
{-# LANGUAGE CPP #-}

module VCard.Symbol.Private.Compat
  ( charSing,
    symbolSing,
    testSCharEquality,
    testSSymbolEquality,
  )
where

#if MIN_VERSION_base(4,18,0)
import VCard.Symbol.Private.Compat.New
  ( charSing,
    symbolSing,
    testSCharEquality,
    testSSymbolEquality,
  )
#else
import VCard.Symbol.Private.Compat.Old
  ( charSing,
    symbolSing,
    testSCharEquality,
    testSSymbolEquality,
  )
#endif
