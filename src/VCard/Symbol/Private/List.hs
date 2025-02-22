-- SPDX-FileCopyrightText: Copyright Preetham Gujjula
-- SPDX-License-Identifier: BSD-3-Clause
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module VCard.Symbol.Private.List
  ( ToList,
  )
where

import GHC.TypeLits (Symbol, UnconsSymbol)

-- | Convert a 'Symbol' to a list of 'Char'.
type family ToList (s :: Symbol) :: [Char] where
  ToList s = ToListUncons (UnconsSymbol s)

type family ToListUncons (x :: (Maybe (Char, Symbol))) :: [Char] where
  ToListUncons Nothing = '[]
  ToListUncons (Just '(c, s)) = c : ToList s
