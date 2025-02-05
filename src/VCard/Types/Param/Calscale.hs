-- SPDX-FileCopyrightText: Copyright Preetham Gujjula
-- SPDX-License-Identifier: BSD-3-Clause
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeOperators #-}

module VCard.Types.Param.Calscale
  ( CalscaleParam (..),
    CalscaleValue (..),
    CalscaleValueSymbol,
  )
where

import GHC.TypeLits (SSymbol)
import VCard.Internal.Symbol (ToUpper)
import VCard.Types.Param.Calscale.Internal.CalscaleValueSymbol (CalscaleValueSymbol)

data CalscaleParam where
  CalscaleParam ::
    (ToUpper param_name ~ "CALSCALE") =>
    { calscaleParamName :: SSymbol param_name,
      calscaleParamValue :: CalscaleValue
    } ->
    CalscaleParam

data CalscaleValue where
  CalscaleValue :: (CalscaleValueSymbol s) => SSymbol s -> CalscaleValue
