-- SPDX-FileCopyrightText: Copyright Preetham Gujjula
-- SPDX-License-Identifier: BSD-3-Clause
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}

module VCard.Types.Param.Pref
  ( Pref,
    PrefValue (..),
  )
where

import Control.Monad (when)
import Data.Finite (Finite, getFinite, packFinite)
import VCard.Parse (HasParser, Parser, parser)
import VCard.Serialize (HasSerializer, Serializer, serializer)
import VCard.Types.Param.Generic (Param)
import VCard.Types.Value.Integer (naturalP)
import VCard.Util (intToText)

type Pref = Param "PREF" PrefValue

-- | 'PrefValue' is 0-indexed, so in this context `finite 0` represents 1 and
--   `finite 99` represents 100
newtype PrefValue = PrefValue {unPrefValue :: Finite 100}
  deriving (Eq, Show, Ord)

instance HasParser PrefValue where
  parser :: Parser PrefValue
  parser = do
    (numLeadingZeros, n) <- naturalP
    when (numLeadingZeros /= 0) (fail "PrefValue: Leading zeros not allowed")

    case packFinite (toInteger n - 1) of
      Nothing -> fail "PrefValue: n out of bounds"
      Just f -> pure (PrefValue f)

instance HasSerializer PrefValue where
  serializer :: Serializer PrefValue
  serializer = intToText . (+ 1) . getFinite . unPrefValue
