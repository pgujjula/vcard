-- SPDX-FileCopyrightText: Copyright Preetham Gujjula
-- SPDX-License-Identifier: BSD-3-Clause
{-# LANGUAGE CPP #-}

module VCard.Util.Symbol.Private.Compat
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

#if MIN_VERSION_base(4,18,0)
import VCard.Util.Symbol.Private.Compat.New
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
#else
import VCard.Util.Symbol.Private.Compat.Old
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
#endif
