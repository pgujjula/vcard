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
{-# OPTIONS_GHC -Wno-all #-}
module VCard.Types.VCard
  ( -- * VCard
    VCard,
    version,

    -- * VCardEntity
    VCardEntity (..),

    -- * VCard1
    VCard1 (..),
  )
where

import VCard.Types.Property (Begin)
import VCard.Types.VCard.Internal (VCard, VCardEntity (..), version)
import Data.Sequence qualified as Seq
import Data.Sequence (Seq)
import Data.IntMap (IntMap)
import Data.IntMap qualified as IntMap

data SomeProperty

data VCard1
  = VCard1
  { vcardPropOrder :: Seq Int
  , vcardPropTable :: IntMap SomeProperty
  , vcardBegin :: Int
  , vcardEnd :: Int
  , vcardSource :: [Int]
  }

getBegin :: VCard1 -> Begin
getBegin = undefined

getBeginRef :: VCard -> Int
getBeginRef = undefined

editBegin :: VCard -> Int -> (Begin -> Begin) -> Maybe VCard
editBegin = undefined

appendProperty :: VCard -> SomeProperty -> Maybe VCard
appendProperty = undefined
