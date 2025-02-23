-- SPDX-FileCopyrightText: Copyright Preetham Gujjula
-- SPDX-License-Identifier: BSD-3-Clause
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module VCard.Symbol.Private.Prefix
  ( IsPrefixOf,
    sIsPrefixOf,
    IsPrefixOfInsensitive,
    sIsPrefixOfInsensitive,
  )
where

import Data.Bool.Singletons (SBool (SFalse, STrue), (%&&))
import Data.Eq.Singletons (PEq (type (==)), (%==))
import Data.List.Singletons (SList (SCons, SNil))
import Data.Type.Bool (type (&&))
import GHC.TypeLits (Symbol)
import VCard.Symbol.Private.Case (ToLower, sToLower)
import VCard.Symbol.Private.Compat (SSymbol)
import VCard.Symbol.Private.List (ToList, sToList)

-- | Determine whether @s@ is a prefix of @t@.
type family IsPrefixOf (s :: Symbol) (t :: Symbol) :: Bool where
  IsPrefixOf s t = IsPrefixOfList (ToList s) (ToList t)

-- | Singleton of 'IsPrefixOf'.
sIsPrefixOf :: SSymbol s -> SSymbol t -> SBool (IsPrefixOf s t)
sIsPrefixOf ss st = sIsPrefixOfList (sToList ss) (sToList st)

-- | Like 'IsPrefixOf', but case-insensitive
type family IsPrefixOfInsensitive (s :: Symbol) (t :: Symbol) :: Bool where
  IsPrefixOfInsensitive s t = IsPrefixOf (ToLower s) (ToLower t)

-- | Singleton of 'IsPrefixOfInsensitive'.
sIsPrefixOfInsensitive ::
  SSymbol s -> SSymbol t -> SBool (IsPrefixOfInsensitive s t)
sIsPrefixOfInsensitive ss st = sIsPrefixOf (sToLower ss) (sToLower st)

type family IsPrefixOfList (s :: [Char]) (t :: [Char]) :: Bool where
  IsPrefixOfList '[] '[] = True
  IsPrefixOfList '[] (y : ys) = True
  IsPrefixOfList (x : xs) (y : ys) = (x == y) && IsPrefixOfList xs ys
  IsPrefixOfList (x : xs) '[] = False

sIsPrefixOfList ::
  SList (s :: [Char]) -> SList (t :: [Char]) -> SBool (IsPrefixOfList s t)
sIsPrefixOfList SNil SNil = STrue
sIsPrefixOfList SNil (SCons _ _) = STrue
sIsPrefixOfList (SCons sx sxs) (SCons sy sys) =
  (sx %== sy) %&& sIsPrefixOfList sxs sys
sIsPrefixOfList (SCons _ _) SNil = SFalse
