-- SPDX-FileCopyrightText: Copyright Preetham Gujjula
-- SPDX-License-Identifier: BSD-3-Clause
{-# LANGUAGE StandaloneKindSignatures #-}

module VCard.Symbol.Private.Singleton
  ( SingletonSymbol,
    sSingletonSymbol,
  )
where

import GHC.TypeLits (ConsSymbol, Symbol)
import VCard.Symbol.Private.Compat (SChar, SSymbol, sConsSymbol, symbolSing)

type SingletonSymbol :: Char -> Symbol
type SingletonSymbol c = ConsSymbol c ""

sSingletonSymbol :: SChar c -> SSymbol (SingletonSymbol c)
sSingletonSymbol sc = sConsSymbol sc (symbolSing @"")
