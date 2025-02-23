-- SPDX-FileCopyrightText: Copyright Preetham Gujjula
-- SPDX-License-Identifier: BSD-3-Clause
{-# LANGUAGE CPP #-}

module VCard.Natural.Private.Compat
  ( SNat,
    natSing,
  )
where

#if MIN_VERSION_base(4,18,0)
import VCard.Natural.Private.Compat.New
  ( SNat,
    natSing,
  )
#else
import VCard.Natural.Private.Compat.Old
  ( SNat,
    natSing,
  )
#endif
