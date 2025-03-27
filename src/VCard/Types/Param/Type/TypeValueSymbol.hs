-- SPDX-FileCopyrightText: Copyright Preetham Gujjula
-- SPDX-License-Identifier: BSD-3-Clause
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-}

module VCard.Types.Param.Type.TypeValueSymbol
  ( TypeGeneralSymbol,
    testTypeGeneralSymbol,
    TypeTelSymbol,
    testTypeTelSymbol,
    TypeRelatedSymbol,
    testTypeRelatedSymbol,
  )
where

import Data.Bool.Singletons (If, SBool (SFalse, STrue), (%||))
import Data.Constraint (Dict (..))
import Data.Kind (Constraint)
import Data.List.Singletons (Elem, SList (SCons, SNil), sElem)
import Data.Proxy (Proxy (..))
import Data.Type.Bool (type (||))
import GHC.TypeLits (ErrorMessage (ShowType, Text, (:<>:)), Symbol, TypeError)
import VCard.Internal.Closed (Closed)
import VCard.Symbol.Private (SSymbol, symbolSing)
import VCard.Types.Textual (IsXNameLowerSymbol, sIsXNameLowerSymbol)

class TypeGeneralSymbol (s :: Symbol) where
  default _privateTypeGeneralSymbol :: (Closed TypeGeneralSymbol) => Proxy s
  _privateTypeGeneralSymbol :: Proxy s
  _privateTypeGeneralSymbol = Proxy

instance (TypeGeneralSymbolInternal s) => TypeGeneralSymbol s where
  _privateTypeGeneralSymbol = Proxy

testTypeGeneralSymbol :: SSymbol s -> Maybe (Dict (TypeGeneralSymbol s))
testTypeGeneralSymbol ss =
  case testTypeGeneralSymbolInternal ss of
    Nothing -> Nothing
    Just Dict -> Just Dict

class TypeTelSymbol (s :: Symbol) where
  default _privateTypeTelSymbol :: (Closed TypeTelSymbol) => Proxy s
  _privateTypeTelSymbol :: Proxy s
  _privateTypeTelSymbol = Proxy

testTypeTelSymbol :: SSymbol s -> Maybe (Dict (TypeTelSymbol s))
testTypeTelSymbol ss =
  case testTypeTelSymbolInternal ss of
    Nothing -> Nothing
    Just Dict -> Just Dict

instance (TypeTelSymbolInternal s) => TypeTelSymbol s where
  _privateTypeTelSymbol = Proxy

class TypeRelatedSymbol (s :: Symbol) where
  default _privateTypeRelatedSymbol :: (Closed TypeRelatedSymbol) => Proxy s
  _privateTypeRelatedSymbol :: Proxy s
  _privateTypeRelatedSymbol = Proxy

testTypeRelatedSymbol :: SSymbol s -> Maybe (Dict (TypeRelatedSymbol s))
testTypeRelatedSymbol ss =
  case testTypeRelatedSymbolInternal ss of
    Nothing -> Nothing
    Just Dict -> Just Dict

instance (TypeRelatedSymbolInternal s) => TypeRelatedSymbol s where
  _privateTypeRelatedSymbol = Proxy

type TypeGeneralSymbolInternal (s :: Symbol) =
  If (IsTypeGeneralSymbol s) Valid (NoInstance "TypeGeneralSymbol" s)

testTypeGeneralSymbolInternal ::
  SSymbol s -> Maybe (Dict (TypeGeneralSymbolInternal s))
testTypeGeneralSymbolInternal ss =
  case sIsTypeGeneralSymbol ss of
    STrue -> Just Dict
    SFalse -> Nothing

type TypeTelSymbolInternal (s :: Symbol) =
  If (IsTypeTelSymbol s) Valid (NoInstance "TypeTelSymbol" s)

testTypeTelSymbolInternal :: SSymbol s -> Maybe (Dict (TypeTelSymbolInternal s))
testTypeTelSymbolInternal ss =
  case sIsTypeTelSymbol ss of
    STrue -> Just Dict
    SFalse -> Nothing

type TypeRelatedSymbolInternal (s :: Symbol) =
  If (IsTypeRelatedSymbol s) Valid (NoInstance "TypeRelatedSymbol" s)

testTypeRelatedSymbolInternal ::
  SSymbol s -> Maybe (Dict (TypeRelatedSymbolInternal s))
testTypeRelatedSymbolInternal ss =
  case sIsTypeRelatedSymbol ss of
    STrue -> Just Dict
    SFalse -> Nothing

type IsTypeGeneralSymbol (s :: Symbol) =
  IsTypeGeneralLiteral s || IsXNameLowerSymbol s

sIsTypeGeneralSymbol :: SSymbol s -> SBool (IsTypeGeneralSymbol s)
sIsTypeGeneralSymbol ss = sIsTypeGeneralLiteral ss %|| sIsXNameLowerSymbol ss

type IsTypeRelatedSymbol (s :: Symbol) =
  IsTypeRelatedLiteral s || IsTypeGeneralLiteral s || IsXNameLowerSymbol s

sIsTypeRelatedSymbol :: SSymbol s -> SBool (IsTypeRelatedSymbol s)
sIsTypeRelatedSymbol ss =
  sIsTypeRelatedLiteral ss
    %|| sIsTypeGeneralLiteral ss
    %|| sIsXNameLowerSymbol ss

type IsTypeTelSymbol (s :: Symbol) =
  IsTypeTelLiteral s || IsTypeGeneralLiteral s || IsXNameLowerSymbol s

sIsTypeTelSymbol :: SSymbol s -> SBool (IsTypeTelSymbol s)
sIsTypeTelSymbol ss =
  sIsTypeTelLiteral ss %|| sIsTypeGeneralLiteral ss %|| sIsXNameLowerSymbol ss

type IsTypeGeneralLiteral (s :: Symbol) =
  Elem s ["work", "home"]

sIsTypeGeneralLiteral :: SSymbol s -> SBool (IsTypeGeneralLiteral s)
sIsTypeGeneralLiteral ss =
  sElem
    ss
    ( (symbolSing @"work")
        `SCons` (symbolSing @"home")
        `SCons` SNil
    )

type IsTypeTelLiteral (s :: Symbol) =
  Elem
    s
    [ "text",
      "voice",
      "fax",
      "cell",
      "video",
      "pager",
      "textphone"
    ]

sIsTypeTelLiteral :: SSymbol s -> SBool (IsTypeTelLiteral s)
sIsTypeTelLiteral ss =
  sElem
    ss
    ( (symbolSing @"text")
        `SCons` (symbolSing @"voice")
        `SCons` (symbolSing @"fax")
        `SCons` (symbolSing @"cell")
        `SCons` (symbolSing @"video")
        `SCons` (symbolSing @"pager")
        `SCons` (symbolSing @"textphone")
        `SCons` SNil
    )

type IsTypeRelatedLiteral (s :: Symbol) =
  Elem
    s
    [ "contact",
      "acquaintance",
      "friend",
      "met",
      "co-worker",
      "colleague",
      "co-resident",
      "neighbor",
      "child",
      "parent",
      "sibling",
      "spouse",
      "kin",
      "muse",
      "crush",
      "date",
      "sweetheart",
      "me",
      "agent",
      "emergency"
    ]

sIsTypeRelatedLiteral :: SSymbol s -> SBool (IsTypeRelatedLiteral s)
sIsTypeRelatedLiteral ss =
  sElem
    ss
    ( (symbolSing @"contact")
        `SCons` (symbolSing @"acquaintance")
        `SCons` (symbolSing @"friend")
        `SCons` (symbolSing @"met")
        `SCons` (symbolSing @"co-worker")
        `SCons` (symbolSing @"colleague")
        `SCons` (symbolSing @"co-resident")
        `SCons` (symbolSing @"neighbor")
        `SCons` (symbolSing @"child")
        `SCons` (symbolSing @"parent")
        `SCons` (symbolSing @"sibling")
        `SCons` (symbolSing @"spouse")
        `SCons` (symbolSing @"kin")
        `SCons` (symbolSing @"muse")
        `SCons` (symbolSing @"crush")
        `SCons` (symbolSing @"date")
        `SCons` (symbolSing @"sweetheart")
        `SCons` (symbolSing @"me")
        `SCons` (symbolSing @"agent")
        `SCons` (symbolSing @"emergency")
        `SCons` SNil
    )

-- Utilities
type family NoInstance (c :: Symbol) (s :: Symbol) where
  NoInstance c s =
    TypeError
      ( Text "No instance for ("
          :<>: Text c
          :<>: Text " "
          :<>: ShowType s
          :<>: Text ")"
      )

type Valid :: Constraint
type Valid = ()
