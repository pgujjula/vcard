-- SPDX-FileCopyrightText: Copyright Preetham Gujjula
-- SPDX-License-Identifier: BSD-3-Clause
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module VCard.Symbol.Private.Snoc
  ( SnocSymbol,
    sSnocSymbol,
    UnsnocSymbol,
    sUnsnocSymbol,
  )
where

import Data.Maybe.Singletons (SMaybe (SJust, SNothing))
import Data.Tuple.Singletons (STuple2 (..))
import GHC.TypeLits (AppendSymbol, ConsSymbol, Symbol, UnconsSymbol)
import VCard.Symbol.Private.Append (sAppendSymbol)
import VCard.Symbol.Private.Compat
  ( SChar,
    SSymbol,
    sConsSymbol,
    sUnconsSymbol,
    symbolSing,
  )
import VCard.Symbol.Private.Singleton (SingletonSymbol, sSingletonSymbol)

-- | Append a 'Char' to the end of a 'Symbol'.
type SnocSymbol (c :: Char) (s :: Symbol) = AppendSymbol s (SingletonSymbol c)

-- | Singleton for 'SnocSymbol'.
sSnocSymbol :: SSymbol s -> SChar c -> SSymbol (SnocSymbol c s)
sSnocSymbol ss sc = sAppendSymbol ss (sSingletonSymbol sc)

-- | Pop a character from the end of a 'Symbol'.
type UnsnocSymbol :: Symbol -> Maybe (Symbol, Char)
type UnsnocSymbol s = UnsnocSymbolUncons (UnconsSymbol s)

-- | Singleton for 'UnsnocSymbol'.
sUnsnocSymbol :: SSymbol s -> SMaybe (UnsnocSymbol s)
sUnsnocSymbol ss = sUnsnocSymbolUncons (sUnconsSymbol ss)

type family UnsnocSymbolUncons (x :: Maybe (Char, Symbol)) :: Maybe (Symbol, Char) where
  UnsnocSymbolUncons Nothing = Nothing
  UnsnocSymbolUncons (Just '(c, s)) = Just (ConsSymbolUnsnoc c (UnsnocSymbol s))

sUnsnocSymbolUncons :: SMaybe x -> SMaybe (UnsnocSymbolUncons x)
sUnsnocSymbolUncons = \case
  SNothing -> SNothing
  SJust (STuple2 sc ss) -> SJust (sConsSymbolUnsnoc sc (sUnsnocSymbol ss))

type family ConsSymbolUnsnoc (c :: Char) (x :: Maybe (Symbol, Char)) :: (Symbol, Char) where
  ConsSymbolUnsnoc c Nothing = '("", c)
  ConsSymbolUnsnoc c (Just '(s, c')) = '(ConsSymbol c s, c')

sConsSymbolUnsnoc :: SChar c -> SMaybe x -> STuple2 (ConsSymbolUnsnoc c x)
sConsSymbolUnsnoc sc = \case
  SNothing -> STuple2 (symbolSing @"") sc
  SJust (STuple2 ss sc') -> STuple2 (sConsSymbol sc ss) sc'
