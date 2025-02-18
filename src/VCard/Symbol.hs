-- SPDX-FileCopyrightText: Copyright Preetham Gujjula
-- SPDX-License-Identifier: BSD-3-Clause

-- | Utilities for working with t'GHC.TypeLits.Symbol' and
--   t'GHC.TypeLits.SSymbol'.
module VCard.Symbol
  ( -- * Convert to lowercase
    ToLower,
    sToLower,

    -- * Convert to uppercase
    ToUpper,
    sToUpper,
  )
where

import VCard.Symbol.Internal
  ( ToLower,
    ToUpper,
    sToLower,
    sToUpper,
  )
