-- SPDX-FileCopyrightText: Copyright Preetham Gujjula
-- SPDX-License-Identifier: BSD-3-Clause
{-# LANGUAGE DataKinds #-}

module VCard.Types.Param.Pref
  ( PrefParam (..),
  )
where

import Data.Finite (Finite)

newtype PrefParam = PrefParam {unPrefParam :: Finite 100}
  deriving (Eq, Show, Ord)
