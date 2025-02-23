-- SPDX-FileCopyrightText: Copyright Preetham Gujjula
-- SPDX-License-Identifier: BSD-3-Clause

module VCard.Natural.Private.Compat.Old (SNat, natSing) where

import GHC.TypeLits (KnownNat)
import GHC.TypeLits.Singletons (SNat (..))

natSing :: (KnownNat n) => SNat n
natSing = SNat
