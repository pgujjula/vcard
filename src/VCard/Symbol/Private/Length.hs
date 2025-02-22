-- SPDX-FileCopyrightText: Copyright Preetham Gujjula
-- SPDX-License-Identifier: BSD-3-Clause
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module VCard.Symbol.Private.Length
  ( Length,
  )
where

import GHC.TypeLits (Symbol, type (+))
import Numeric.Natural (Natural)
import VCard.Symbol.Private.List (ToList)

type family Length (s :: Symbol) :: Natural where
  Length s = ListLength (ToList s)

type family ListLength (xs :: [a]) :: Natural where
  ListLength '[] = 0
  ListLength (x : xs) = 1 + ListLength xs
