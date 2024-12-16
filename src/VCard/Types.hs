-- SPDX-FileCopyrightText: Copyright Preetham Gujjula
-- SPDX-License-Identifier: BSD-3-Clause

module VCard.Types
  ( VCard (..),
    VCardEntity (..),
    FN (..),
    Version (..),
  )
where

import Data.List.NonEmpty (NonEmpty)
import Data.Text (Text)

newtype VCardEntity = VCardEntity {unVCardEntity :: NonEmpty VCard}
  deriving (Eq, Show, Ord)

data VCard = VCard
  { vCardVersion :: Version,
    vCardFN :: FN
  }
  deriving (Eq, Show, Ord)

data Version = Version_4_0
  deriving (Eq, Show, Ord)

newtype FN = FN {unFN :: Text}
  deriving (Eq, Show, Ord)
