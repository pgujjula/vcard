-- SPDX-FileCopyrightText: Copyright Preetham Gujjula
-- SPDX-License-Identifier: BSD-3-Clause
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeOperators #-}

module VCard.Types.XName
  ( XNameSymbol,
  )
where

import GHC.TypeLits (Symbol)
import VCard.Internal.Symbol (IsPrefixOfInsensitive)

type XNameSymbol (s :: Symbol) = IsPrefixOfInsensitive "x-" s ~ 'True
