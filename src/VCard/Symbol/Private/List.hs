-- SPDX-FileCopyrightText: Copyright Preetham Gujjula
-- SPDX-License-Identifier: BSD-3-Clause
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module VCard.Symbol.Private.List
  ( ToList,
    sToList,
    FromList,
    sFromList,
  )
where

import Data.List.Singletons (SList (SCons, SNil))
import Data.Maybe.Singletons (SMaybe (SJust, SNothing))
import Data.Tuple.Singletons (STuple2 (..))
import GHC.TypeLits (ConsSymbol, Symbol, UnconsSymbol)
import VCard.Symbol.Private.Compat
  ( SSymbol,
    sConsSymbol,
    sUnconsSymbol,
    symbolSing,
  )

-- | Convert a 'Symbol' to a list of 'Char'.
type family ToList (s :: Symbol) :: [Char] where
  ToList s = ToListUncons (UnconsSymbol s)

-- | Singleton of 'ToList'.
sToList :: SSymbol s -> SList (ToList s)
sToList ss = sToListUncons (sUnconsSymbol ss)

type family ToListUncons (x :: (Maybe (Char, Symbol))) :: [Char] where
  ToListUncons Nothing = '[]
  ToListUncons (Just '(c, s)) = c : ToList s

sToListUncons :: SMaybe (x :: Maybe (Char, Symbol)) -> SList (ToListUncons x)
sToListUncons SNothing = SNil
sToListUncons (SJust (STuple2 sc ss)) = SCons sc (sToList ss)

type family FromList (xs :: [Char]) :: Symbol where
  FromList '[] = ""
  FromList (x : xs) = ConsSymbol x (FromList xs)

sFromList :: SList xs -> SSymbol (FromList xs)
sFromList SNil = symbolSing @""
sFromList (SCons sx sxs) = sConsSymbol sx (sFromList sxs)
