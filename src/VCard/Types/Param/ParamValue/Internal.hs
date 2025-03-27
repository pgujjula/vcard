-- SPDX-FileCopyrightText: Copyright Preetham Gujjula
-- SPDX-License-Identifier: BSD-3-Clause
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module VCard.Types.Param.ParamValue.Internal
  ( IsParamValueSymbol,
    sIsParamValueSymbol,

    -- * QSafe Symbol
    IsQSafeSymbol,
    sIsQSafeSymbol,
    IsQSafeList,
    sIsQSafeList,
    IsQSafeTail,
    sIsQSafeTail,

    -- * Safe Symbol
    IsSafeSymbol,
    sIsSafeSymbol,
    IsSafeList,
    sIsSafeList,

    -- * Unquoting
    UnsafeUnquoteParamValueSymbol,
    sUnsafeUnquoteParamValueSymbol,
  )
where

import Data.Bool.Singletons
  ( SBool (SFalse, STrue),
    (%&&),
    (%||),
    type (&&),
    type (||),
  )
import Data.Eq.Singletons ((%==), type (==))
import Data.List.Singletons (SList (SCons, SNil))
import Data.Type.Bool (If)
import GHC.TypeLits (Symbol, type (-))
import Prelude.Singletons ((%-))
import VCard.Char
  ( DQuote,
    IsQSafeChar,
    IsSafeChar,
    sDQuote,
    sIsQSafeChar,
    sIsSafeChar,
  )
import VCard.Natural.Private (natSing)
import VCard.Util.Symbol
  ( Drop,
    Length,
    SSymbol,
    Take,
    ToList,
    sDrop,
    sLength,
    sTake,
    sToList,
    symbolSing,
  )

type family IsParamValueSymbol (s :: Symbol) :: Bool where
  IsParamValueSymbol s = IsSafeSymbol s || IsQSafeSymbol s

-- | Singleton of 'IsParamValueSymbol'.
sIsParamValueSymbol :: SSymbol s -> SBool (IsParamValueSymbol s)
sIsParamValueSymbol ss = sIsSafeSymbol ss %|| sIsQSafeSymbol ss

--
-- IsQSafeSymbol
--

-- | The 'Symbol' matches the @DQUOTE *QSAFE-CHAR DQUOTE@ pattern in Section 3.3
--   of RFC 6350.
type IsQSafeSymbol (s :: Symbol) = IsQSafeList (ToList s)

-- | Singleton of 'IsQSafeSymbol'.
sIsQSafeSymbol :: SSymbol s -> SBool (IsQSafeSymbol s)
sIsQSafeSymbol ss = sIsQSafeList (sToList ss)

type family IsQSafeList (xs :: [Char]) where
  IsQSafeList '[] = False
  IsQSafeList (x : xs) = x == DQuote && IsQSafeTail xs

-- | Singleton of 'IsQSafeList'.
sIsQSafeList :: SList (xs :: [Char]) -> SBool (IsQSafeList xs)
sIsQSafeList SNil = SFalse
sIsQSafeList (SCons sx sxs) = sx %== sDQuote %&& sIsQSafeTail sxs

type family IsQSafeTail (xs :: [Char]) :: Bool where
  IsQSafeTail '[] = False
  IsQSafeTail (x : '[]) = x == DQuote
  IsQSafeTail (x1 : x2 : xs) = IsQSafeChar x1 && IsQSafeTail (x2 : xs)

-- | Singleton of 'IsQSafeTail'.
sIsQSafeTail :: SList (xs :: [Char]) -> SBool (IsQSafeTail xs)
sIsQSafeTail SNil = SFalse
sIsQSafeTail (SCons sc SNil) = sc %== sDQuote
sIsQSafeTail (SCons sc1 (SCons sc2 scs)) =
  sIsQSafeChar sc1 %&& sIsQSafeTail (SCons sc2 scs)

--
-- IsSafeSymbol
--

-- | The 'Symbol' matches the @*SAFE-CHAR@ pattern in Section 3.3 of RFC 6350.
type IsSafeSymbol (s :: Symbol) = IsSafeList (ToList s)

-- | Singleton of 'IsSafeSymbol'.
sIsSafeSymbol :: SSymbol s -> SBool (IsSafeSymbol s)
sIsSafeSymbol s = sIsSafeList (sToList s)

type family IsSafeList (xs :: [Char]) where
  IsSafeList '[] = True
  IsSafeList (x : xs) = IsSafeChar x && IsSafeList xs

-- | Singleton of 'IsSafeList'.
sIsSafeList :: SList (xs :: [Char]) -> SBool (IsSafeList xs)
sIsSafeList SNil = STrue
sIsSafeList (SCons sx sxs) = sIsSafeChar sx %&& sIsSafeList sxs

--
-- Unquoting
--

-- | Strip quotes from a quoted 'Symbol', or return an unquoted 'Symbol'
--   unchanged. Undefined behavior when the input is improperly quoted.
type UnsafeUnquoteParamValueSymbol :: Symbol -> Symbol
type UnsafeUnquoteParamValueSymbol s =
  If
    (Take 1 s == "\"")
    (Drop 1 (Take (Length s - 1) s))
    s

-- | Singleton of 'UnsafeUnquoteParamValueSymbol'.
sUnsafeUnquoteParamValueSymbol ::
  SSymbol s -> SSymbol (UnsafeUnquoteParamValueSymbol s)
sUnsafeUnquoteParamValueSymbol ss =
  case sTake (natSing @1) ss %== symbolSing @"\"" of
    STrue -> sDrop (natSing @1) (sTake (sLength ss %- natSing @1) ss)
    SFalse -> ss
