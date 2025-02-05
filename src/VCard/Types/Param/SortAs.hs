-- SPDX-FileCopyrightText: Copyright Preetham Gujjula
-- SPDX-License-Identifier: BSD-3-Clause
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeOperators #-}

module VCard.Types.Param.SortAs
  ( SortAsParam (..),
  )
where

import Data.List.NonEmpty (NonEmpty)
import GHC.TypeLits (SSymbol)
import VCard.Internal.Symbol (ToUpper)
import VCard.Types.Param.ParamValue (SomeParamValue)

data SortAsParam where
  SortAsParam ::
    (ToUpper param_name ~ "SORT-AS") =>
    { sortAsParamName :: SSymbol param_name,
      sortAsParamValue :: NonEmpty SomeParamValue
    } ->
    SortAsParam
