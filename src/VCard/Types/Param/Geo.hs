-- SPDX-FileCopyrightText: Copyright Preetham Gujjula
-- SPDX-License-Identifier: BSD-3-Clause
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeOperators #-}

module VCard.Types.Param.Geo
  ( GeoParam (..),
  )
where

import GHC.TypeLits (SSymbol)
import VCard.Internal.Symbol (ToUpper)
import VCard.Types.Value.URI (URI)

data GeoParam where
  GeoParam ::
    (ToUpper param_name ~ "GEO") =>
    { geoParamName :: SSymbol param_name,
      geoParamValue :: URI
    } ->
    GeoParam
