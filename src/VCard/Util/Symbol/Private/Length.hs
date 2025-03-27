-- SPDX-FileCopyrightText: Copyright Preetham Gujjula
-- SPDX-License-Identifier: BSD-3-Clause
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module VCard.Util.Symbol.Private.Length
  ( Length,
    sLength,
  )
where

import Data.List.Singletons (SList (SCons, SNil))
import GHC.TypeLits (Symbol, type (+))
import Numeric.Natural (Natural)
import Prelude.Singletons ((%+))
import VCard.Natural.Private (SNat, natSing)
import VCard.Util.Symbol.Private.Compat (SSymbol)
import VCard.Util.Symbol.Private.List (ToList, sToList)

-- | The length of a 'Symbol'.
type family Length (s :: Symbol) :: Natural where
  Length s = ListLength (ToList s)

-- | Singleton of 'Length'.
sLength :: SSymbol s -> SNat (Length s)
sLength ss = sListLength (sToList ss)

type family ListLength (xs :: [a]) :: Natural where
  ListLength '[] = 0
  ListLength (x : xs) = 1 + ListLength xs

sListLength :: SList (xs :: [a]) -> SNat (ListLength xs)
sListLength SNil = natSing @0
sListLength (SCons _ sxs) = natSing @1 %+ sListLength sxs
