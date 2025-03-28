-- SPDX-FileCopyrightText: Copyright Preetham Gujjula
-- SPDX-License-Identifier: BSD-3-Clause

module VCard.Util.Natural
  ( -- * 'GHC.TypeLits.Nat' singleton
    SNat,
    natSing,
    withKnownNat,
  )
where

import VCard.Util.Natural.Private.Compat (SNat, natSing, withKnownNat)
