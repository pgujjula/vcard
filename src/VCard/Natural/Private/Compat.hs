-- SPDX-FileCopyrightText: Copyright Preetham Gujjula
-- SPDX-License-Identifier: BSD-3-Clause
{-# LANGUAGE CPP #-}

module VCard.Natural.Private.Compat
  ( SNat,
    natSing,
    withKnownNat,
  )
where

#if MIN_VERSION_base(4,18,0)
import VCard.Natural.Private.Compat.New
  ( SNat,
    natSing,
    withKnownNat,
  )
#else
import VCard.Natural.Private.Compat.Old
  ( SNat,
    natSing,
    withKnownNat,
  )
#endif
