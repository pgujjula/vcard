-- SPDX-FileCopyrightText: Copyright Preetham Gujjula
-- SPDX-License-Identifier: BSD-3-Clause

module VCard.Util.Natural.Private.Compat.Old (SNat, natSing, withKnownNat) where

import GHC.TypeLits (KnownNat)
import GHC.TypeLits.Singletons (SNat (..), withKnownNat)

natSing :: (KnownNat n) => SNat n
natSing = SNat
