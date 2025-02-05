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

module VCard.Types.Param.Calscale.Internal.CalscaleValueSymbol (CalscaleValueSymbol) where

import Data.Proxy (Proxy (..))
import VCard.Internal.Closed (Closed)
import VCard.Types.XName (XNameSymbol)

-- | 'CalscaleValueSymbol' is a closed class, attempting to define a new instance
--   will result in a compiler error.
class CalscaleValueSymbol a where
  default _privateCalscaleValueSymbol :: (Closed CalscaleValueSymbol) => Proxy a
  _privateCalscaleValueSymbol :: Proxy a
  _privateCalscaleValueSymbol = Proxy

instance CalscaleValueSymbol "gregorian" where
  _privateCalscaleValueSymbol = Proxy

instance (XNameSymbol s) => CalscaleValueSymbol s where
  _privateCalscaleValueSymbol = Proxy
