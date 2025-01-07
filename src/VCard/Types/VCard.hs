-- SPDX-FileCopyrightText: Copyright Preetham Gujjula
-- SPDX-License-Identifier: BSD-3-Clause

-- |
-- Module     : VCard.Types.VCard
-- Copyright  : Copyright Preetham Gujjula
-- License    : BSD-3-Clause
-- Maintainer : Preetham Gujjula <libraries@mail.preetham.io>
-- Stability  : experimental
--
-- Main 'VCard' and 'VCardEntity' types, described in Section 3.3 of RFC 6350.
module VCard.Types.VCard
  ( -- * VCard
    VCard,
    version,

    -- * VCardEntity
    VCardEntity (..),
  )
where

import VCard.Types.VCard.Internal (VCard, VCardEntity (..), version)
