-- SPDX-FileCopyrightText: Copyright Preetham Gujjula
-- SPDX-License-Identifier: BSD-3-Clause
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeOperators #-}

module VCard.Types.Param.Value
  ( ValueParam (..),
    ValueType (..),
    ValueValueSymbol,
  )
where

import GHC.TypeLits (SSymbol, Symbol)
import VCard.Internal.Symbol (ToUpper)
import VCard.Types.Param.Value.Internal.ValueTypeSymbol (ValueValueSymbol)

data ValueParam (s :: Symbol) where
  ValueParam ::
    (ToUpper param_name ~ "VALUE") =>
    { valueParamName :: SSymbol param_name,
      valueParamType :: ValueType s
    } ->
    ValueParam s

data ValueType (s :: Symbol) where
  ValueType :: (ValueValueSymbol s) => SSymbol s -> ValueType s
