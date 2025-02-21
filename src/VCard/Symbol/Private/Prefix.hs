-- SPDX-FileCopyrightText: Copyright Preetham Gujjula
-- SPDX-License-Identifier: BSD-3-Clause
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module VCard.Symbol.Private.Prefix
  ( IsPrefixOf,
    IsPrefixOfInsensitive,
  )
where

import Data.Type.Bool (type (&&))
import Data.Type.Equality (type (==))
import GHC.TypeLits (Symbol, UnconsSymbol)
import VCard.Symbol.Private.Case (ToLower)

-- | Determine whether @s@ is a prefix of @t@.
type family IsPrefixOf (s :: Symbol) (t :: Symbol) :: Bool where
  IsPrefixOf s t = IsPrefixOfList (ToList s) (ToList t)

-- | Like 'IsPrefixOf', but case-insensitive
type family IsPrefixOfInsensitive (s :: Symbol) (t :: Symbol) :: Bool where
  IsPrefixOfInsensitive s t = IsPrefixOf (ToLower s) (ToLower t)

type family IsPrefixOfList (s :: [Char]) (t :: [Char]) :: Bool where
  IsPrefixOfList '[] '[] = True
  IsPrefixOfList '[] (y : ys) = True
  IsPrefixOfList (x : xs) (y : ys) = (x == y) && IsPrefixOfList xs ys
  IsPrefixOfList (x : xs) '[] = False

type family ToList (s :: Symbol) :: [Char] where
  ToList s = ToListUncons (UnconsSymbol s)

type family ToListUncons (x :: (Maybe (Char, Symbol))) :: [Char] where
  ToListUncons Nothing = '[]
  ToListUncons (Just '(c, s)) = c : ToList s
