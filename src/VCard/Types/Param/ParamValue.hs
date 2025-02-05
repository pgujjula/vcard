-- SPDX-FileCopyrightText: Copyright Preetham Gujjula
-- SPDX-License-Identifier: BSD-3-Clause
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module VCard.Types.Param.ParamValue
  ( SomeParamValue (..),
    ParamValue (..),
    ParamValueSymbol,
    IsParamValueSymbol,
    IsSafeSymbol,
    IsQSafeSymbol,
    IsSafeList,
    IsSafeChar,
    IsQSafeList,
    IsQSafeTail,
    IsWhitespace,
    IsInRange,
    IsNonAscii,
    IsQSafeChar,
    DQuote,
    Sp,
    HTab,
  )
where

import Data.Type.Bool (Not, type (&&), type (||))
import Data.Type.Equality (type (==))
import Data.Type.Ord (type (<=?))
import GHC.TypeLits (SSymbol, Symbol)
import VCard.Internal.Symbol (ToList)

data SomeParamValue where
  SomeParamValue :: ParamValue s -> SomeParamValue

data ParamValue (s :: Symbol) where
  ParamValue ::
    (ParamValueSymbol s) =>
    {unParamValue :: SSymbol s} ->
    ParamValue s

type ParamValueSymbol s = IsParamValueSymbol s ~ 'True

type IsParamValueSymbol s = IsSafeSymbol s || IsQSafeSymbol s

type IsSafeSymbol (s :: Symbol) = IsSafeList (ToList s)

type family IsSafeList (xs :: [Char]) where
  IsSafeList '[] = 'True
  IsSafeList (x ': xs) = IsSafeChar x && IsSafeList xs

type IsSafeChar (c :: Char) =
  IsWhitespace c || c == '!' || IsInRange '( '\x23', '\x7e') c || IsNonAscii c

type IsWhitespace c = c == Sp || c == HTab

type family IsInRange (range :: (Char, Char)) (c :: Char) :: Bool where
  IsInRange '(lo, hi) c = lo <=? c && c <=? hi

type IsNonAscii c = Not (IsInRange '( '\x00', '\x7f') c)

type Sp = '\x20'

type HTab = '\x09'

type IsQSafeSymbol (s :: Symbol) = IsQSafeList (ToList s)

type family IsQSafeList (xs :: [Char]) :: Bool where
  IsQSafeList '[] = 'False
  IsQSafeList (x ': xs) = (x == DQuote) && IsQSafeTail xs

type family IsQSafeTail (xs :: [Char]) :: Bool where
  IsQSafeTail '[] = 'False
  IsQSafeTail '[x] = x == DQuote
  IsQSafeTail (x ': xs) = IsQSafeChar x && IsQSafeTail xs

type DQuote = '\x0022'

type family IsQSafeChar (c :: Char) :: Bool where
