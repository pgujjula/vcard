-- SPDX-FileCopyrightText: Copyright Preetham Gujjula
-- SPDX-License-Identifier: BSD-3-Clause

-- | ⚠️ WARNING: This module is considered internal, and may change without
--   notice. It is exposed primarily for to allow Haddock links to resolve.
module VCard.Symbol.Internal
  ( -- * Change case

    -- ** Convert to lowercase
    ToLowerUncons,
    sToLowerUncons,
    ToLowerChar,
    sToLowerChar,

    -- ** Convert to uppercase
    ToUpperUncons,
    sToUpperUncons,
    ToUpperChar,
    sToUpperChar,
  )
where

import VCard.Symbol.Private
  ( ToLowerChar,
    ToLowerUncons,
    ToUpperChar,
    ToUpperUncons,
    sToLowerChar,
    sToLowerUncons,
    sToUpperChar,
    sToUpperUncons,
  )
