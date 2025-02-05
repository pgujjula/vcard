-- SPDX-FileCopyrightText: Copyright Preetham Gujjula
-- SPDX-License-Identifier: BSD-3-Clause
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-}
{-# OPTIONS_GHC -Wno-simplifiable-class-constraints #-}

module VCard.Types.Param.Value.Internal.ValueTypeSymbol (ValueValueSymbol) where

import Data.Proxy (Proxy (..))
import VCard.Internal.Closed (Closed)
import VCard.Types.XName (XNameSymbol)

-- | 'ValueValueSymbol' is a closed class, attempting to define a new instance
--   will result in a compiler error.
class ValueValueSymbol a where
  default _privateValueValueSymbol :: (Closed ValueValueSymbol) => Proxy a
  _privateValueValueSymbol :: Proxy a
  _privateValueValueSymbol = Proxy

instance ValueValueSymbol "text" where
  _privateValueValueSymbol = Proxy

instance ValueValueSymbol "uri" where
  _privateValueValueSymbol = Proxy

instance ValueValueSymbol "date" where
  _privateValueValueSymbol = Proxy

instance ValueValueSymbol "time" where
  _privateValueValueSymbol = Proxy

instance ValueValueSymbol "date-time" where
  _privateValueValueSymbol = Proxy

instance ValueValueSymbol "date-and-or-time" where
  _privateValueValueSymbol = Proxy

instance ValueValueSymbol "timestamp" where
  _privateValueValueSymbol = Proxy

instance ValueValueSymbol "boolean" where
  _privateValueValueSymbol = Proxy

instance ValueValueSymbol "integer" where
  _privateValueValueSymbol = Proxy

instance ValueValueSymbol "float" where
  _privateValueValueSymbol = Proxy

instance ValueValueSymbol "utc-offset" where
  _privateValueValueSymbol = Proxy

instance ValueValueSymbol "language-tag" where
  _privateValueValueSymbol = Proxy

instance (XNameSymbol s) => ValueValueSymbol s where
  _privateValueValueSymbol = Proxy
