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

module VCard.Types.Param.Type.Internal.TypeValueSymbol
  ( TypeValueSymbol,
    TypeParamTelSymbol,
    TypeParamRelatedSymbol,
  )
where

import Data.Proxy (Proxy (..))
import VCard.Internal.Closed (Closed)
import VCard.Types.XName (XNameSymbol)

-- | 'TypeValueSymbol' is a closed class, attempting to define a new instance
--   will result in a compiler error.
class TypeValueSymbol a where
  default _privateTypeValueSymbol :: (Closed TypeValueSymbol) => Proxy a
  _privateTypeValueSymbol :: Proxy a
  _privateTypeValueSymbol = Proxy

instance TypeValueSymbol "work" where
  _privateTypeValueSymbol = Proxy

instance TypeValueSymbol "home" where
  _privateTypeValueSymbol = Proxy

instance (XNameSymbol s) => TypeValueSymbol s where
  _privateTypeValueSymbol = Proxy

class TypeParamTelSymbol a where
  default _privateTypeParamTelSymbol :: (Closed TypeParamTelSymbol) => Proxy a
  _privateTypeParamTelSymbol :: Proxy a
  _privateTypeParamTelSymbol = Proxy

instance TypeParamTelSymbol "text" where
  _privateTypeParamTelSymbol = Proxy

instance TypeParamTelSymbol "voice" where
  _privateTypeParamTelSymbol = Proxy

instance TypeParamTelSymbol "fax" where
  _privateTypeParamTelSymbol = Proxy

instance TypeParamTelSymbol "cell" where
  _privateTypeParamTelSymbol = Proxy

instance TypeParamTelSymbol "video" where
  _privateTypeParamTelSymbol = Proxy

instance TypeParamTelSymbol "pager" where
  _privateTypeParamTelSymbol = Proxy

instance TypeParamTelSymbol "textphone" where
  _privateTypeParamTelSymbol = Proxy

instance (TypeValueSymbol s) => TypeParamTelSymbol s where
  _privateTypeParamTelSymbol = Proxy

class TypeParamRelatedSymbol a where
  default _privateTypeParamRelatedSymbol :: (Closed TypeParamRelatedSymbol) => Proxy a
  _privateTypeParamRelatedSymbol :: Proxy a
  _privateTypeParamRelatedSymbol = Proxy

instance TypeParamRelatedSymbol "contact" where
  _privateTypeParamRelatedSymbol = Proxy

instance TypeParamRelatedSymbol "acquaintance" where
  _privateTypeParamRelatedSymbol = Proxy

instance TypeParamRelatedSymbol "friend" where
  _privateTypeParamRelatedSymbol = Proxy

instance TypeParamRelatedSymbol "met" where
  _privateTypeParamRelatedSymbol = Proxy

instance TypeParamRelatedSymbol "co-worker" where
  _privateTypeParamRelatedSymbol = Proxy

instance TypeParamRelatedSymbol "colleague" where
  _privateTypeParamRelatedSymbol = Proxy

instance TypeParamRelatedSymbol "co-resident" where
  _privateTypeParamRelatedSymbol = Proxy

instance TypeParamRelatedSymbol "neighbor" where
  _privateTypeParamRelatedSymbol = Proxy

instance TypeParamRelatedSymbol "child" where
  _privateTypeParamRelatedSymbol = Proxy

instance TypeParamRelatedSymbol "parent" where
  _privateTypeParamRelatedSymbol = Proxy

instance TypeParamRelatedSymbol "sibling" where
  _privateTypeParamRelatedSymbol = Proxy

instance TypeParamRelatedSymbol "spouse" where
  _privateTypeParamRelatedSymbol = Proxy

instance TypeParamRelatedSymbol "kin" where
  _privateTypeParamRelatedSymbol = Proxy

instance TypeParamRelatedSymbol "muse" where
  _privateTypeParamRelatedSymbol = Proxy

instance TypeParamRelatedSymbol "crush" where
  _privateTypeParamRelatedSymbol = Proxy

instance TypeParamRelatedSymbol "date" where
  _privateTypeParamRelatedSymbol = Proxy

instance TypeParamRelatedSymbol "sweetheart" where
  _privateTypeParamRelatedSymbol = Proxy

instance TypeParamRelatedSymbol "me" where
  _privateTypeParamRelatedSymbol = Proxy

instance TypeParamRelatedSymbol "agent" where
  _privateTypeParamRelatedSymbol = Proxy

instance TypeParamRelatedSymbol "emergency" where
  _privateTypeParamRelatedSymbol = Proxy

instance (TypeValueSymbol s) => TypeParamRelatedSymbol s where
  _privateTypeParamRelatedSymbol = Proxy
