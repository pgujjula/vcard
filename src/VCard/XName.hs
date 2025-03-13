-- SPDX-FileCopyrightText: Copyright Preetham Gujjula
-- SPDX-License-Identifier: BSD-3-Clause
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module VCard.XName
  ( XNameSymbol,
    IsXNameSymbol,
    sIsXNameSymbol,
    testXNameSymbol,
    XNameLowerSymbol,
    IsXNameLowerSymbol,
    sIsXNameLowerSymbol,
    testXNameLowerSymbol,
    XNameUpperSymbol,
    IsXNameUpperSymbol,
    sIsXNameUpperSymbol,
    testXNameUpperSymbol,
  )
where

import Data.Bool.Singletons (SBool (SFalse, STrue), (%&&))
import Data.Constraint (Dict (..))
import Data.Kind (Constraint)
import Data.Ord.Singletons ((%>), type (>))
import Data.Type.Bool (type (&&))
import GHC.TypeError (Assert)
import GHC.TypeLits (Symbol)
import VCard.AlphaNumDash
  ( IsAlphaNumDashLowerSymbol,
    IsAlphaNumDashSymbol,
    IsAlphaNumDashUpperSymbol,
    sIsAlphaNumDashLowerSymbol,
    sIsAlphaNumDashSymbol,
    sIsAlphaNumDashUpperSymbol,
  )
import VCard.Natural.Private (natSing)
import VCard.Symbol.Private
  ( IsPrefixOf,
    IsPrefixOfInsensitive,
    Length,
    SSymbol,
    sIsPrefixOf,
    sIsPrefixOfInsensitive,
    sLength,
    symbolSing,
  )
import VCard.Util (NoInstance)

-- Writing XNameSymbol/XNameSymbolLower/XNameSymbolUpper as type synonyms does
-- not work on GHC 9.2. Once we drop support for GHC 9.2 we can rewrite them as
-- type synonyms.
type family XNameSymbol (s :: Symbol) :: Constraint where
  XNameSymbol s = Assert (IsXNameSymbol s) (NoInstance "XNameSymbol" s)

testXNameSymbol :: SSymbol s -> Maybe (Dict (XNameSymbol s))
testXNameSymbol ss =
  case sIsXNameSymbol ss of
    STrue -> Just Dict
    SFalse -> Nothing

type IsXNameSymbol s =
  IsPrefixOfInsensitive "x-" s && Length s > 2 && IsAlphaNumDashSymbol s

sIsXNameSymbol :: SSymbol s -> SBool (IsXNameSymbol s)
sIsXNameSymbol ss =
  sIsPrefixOfInsensitive (symbolSing @"x-") ss
    %&& sLength ss %> natSing @2
    %&& sIsAlphaNumDashSymbol ss

type family XNameLowerSymbol (s :: Symbol) :: Constraint where
  XNameLowerSymbol s =
    Assert (IsXNameLowerSymbol s) (NoInstance "XNameLowerSymbol" s)

testXNameLowerSymbol :: SSymbol s -> Maybe (Dict (XNameLowerSymbol s))
testXNameLowerSymbol ss =
  case sIsXNameLowerSymbol ss of
    STrue -> Just Dict
    SFalse -> Nothing

type IsXNameLowerSymbol s =
  IsPrefixOf "x-" s && Length s > 2 && IsAlphaNumDashLowerSymbol s

sIsXNameLowerSymbol :: SSymbol s -> SBool (IsXNameLowerSymbol s)
sIsXNameLowerSymbol ss =
  sIsPrefixOf (symbolSing @"x-") ss
    %&& sLength ss %> natSing @2
    %&& sIsAlphaNumDashLowerSymbol ss

type family XNameUpperSymbol (s :: Symbol) :: Constraint where
  XNameUpperSymbol s =
    Assert (IsXNameUpperSymbol s) (NoInstance "XNameUpperSymbol" s)

testXNameUpperSymbol :: SSymbol s -> Maybe (Dict (XNameUpperSymbol s))
testXNameUpperSymbol ss =
  case sIsXNameUpperSymbol ss of
    STrue -> Just Dict
    SFalse -> Nothing

type IsXNameUpperSymbol s =
  IsPrefixOf "X-" s && Length s > 2 && IsAlphaNumDashUpperSymbol s

sIsXNameUpperSymbol :: SSymbol s -> SBool (IsXNameUpperSymbol s)
sIsXNameUpperSymbol ss =
  sIsPrefixOf (symbolSing @"X-") ss
    %&& sLength ss %> natSing @2
    %&& sIsAlphaNumDashUpperSymbol ss
