-- SPDX-FileCopyrightText: Copyright Preetham Gujjula
-- SPDX-License-Identifier: BSD-3-Clause
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeOperators #-}

module VCard.Types.Param.TZ
  ( TZParam (..),
    TZValue (..),
  )
where

import GHC.TypeLits (SSymbol)
import VCard.Internal.Symbol (ToUpper)
import VCard.Types.Param.ParamValue (SomeParamValue)
import VCard.Types.Value.URI (URI)
import Vary (Vary)

data TZParam where
  TZParam ::
    (ToUpper param_name ~ "TZ") =>
    { tzParamName :: SSymbol param_name,
      tzParamValue :: TZValue
    } ->
    TZParam

newtype TZValue = TZValue
  {unTZValue :: Vary '[SomeParamValue, URI]}
