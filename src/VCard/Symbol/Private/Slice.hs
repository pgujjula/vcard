-- SPDX-FileCopyrightText: Copyright Preetham Gujjula
-- SPDX-License-Identifier: BSD-3-Clause

module VCard.Symbol.Private.Slice
  ( Take,
    sTake,
    Drop,
    sDrop,
  )
where

import Data.List.Singletons qualified as List
import GHC.TypeLits (Nat, Symbol)
import VCard.Natural.Private (SNat)
import VCard.Symbol.Private.Compat (SSymbol)
import VCard.Symbol.Private.List (FromList, ToList, sFromList, sToList)

type Take (n :: Nat) (s :: Symbol) = FromList (List.Take n (ToList s))

sTake :: SNat n -> SSymbol s -> SSymbol (Take n s)
sTake sn ss = sFromList (List.sTake sn (sToList ss))

type Drop (n :: Nat) (s :: Symbol) = FromList (List.Drop n (ToList s))

sDrop :: SNat n -> SSymbol s -> SSymbol (Drop n s)
sDrop sn ss = sFromList (List.sDrop sn (sToList ss))
