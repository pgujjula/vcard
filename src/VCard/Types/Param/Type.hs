-- SPDX-FileCopyrightText: Copyright Preetham Gujjula
-- SPDX-License-Identifier: BSD-3-Clause
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeOperators #-}

module VCard.Types.Param.Type
  ( TypeParam (..),
    TypeValue (..),
    TypeValueSymbol,
  )
where

import GHC.TypeLits (SSymbol, Symbol)
import VCard.Internal.Symbol (ToUpper)
import VCard.Types.Param.Type.Internal.TypeValueSymbol (TypeValueSymbol)

data TypeParam (s :: Symbol) where
  TypeParam ::
    (ToUpper param_name ~ "TYPE") =>
    { typeParamName :: SSymbol param_name,
      typeParamValue :: TypeValue s
    } ->
    TypeParam s

data TypeValue (s :: Symbol) where
  TypeValue :: (TypeValueSymbol s) => SSymbol s -> TypeValue s
