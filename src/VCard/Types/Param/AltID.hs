-- SPDX-FileCopyrightText: Copyright Preetham Gujjula
-- SPDX-License-Identifier: BSD-3-Clause
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeOperators #-}

module VCard.Types.Param.AltID
  ( AltIDParam (..),
  )
where

import GHC.TypeLits (SSymbol, Symbol)
import VCard.Internal.Symbol (ToUpper)
import VCard.Types.Param.ParamValue (ParamValue)

data AltIDParam (param_value :: Symbol) where
  AltIDParam ::
    (ToUpper param_name ~ "AltID") =>
    { altIDParamName :: SSymbol param_name,
      altIDParamValue :: ParamValue param_value
    } ->
    AltIDParam s
