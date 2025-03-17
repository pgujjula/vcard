-- SPDX-FileCopyrightText: Copyright Preetham Gujjula
-- SPDX-License-Identifier: BSD-3-Clause
{-# OPTIONS_GHC -Wno-orphans #-}

module VCard.Symbol.Private.Serialize () where

import Data.Text qualified as Text
import VCard.Serialize (HasSerializer, Serializer, serializer)
import VCard.Symbol.Private.Compat (SSymbol, fromSSymbol)

instance HasSerializer (SSymbol s) where
  serializer :: Serializer (SSymbol s)
  serializer = Text.pack . fromSSymbol
