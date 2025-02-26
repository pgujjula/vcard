{-# LANGUAGE AllowAmbiguousTypes #-}
-- SPDX-FileCopyrightText: Copyright Preetham Gujjula
-- SPDX-License-Identifier: BSD-3-Clause
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fplugin=Data.Record.Anon.Plugin #-}

module VCard.Types.Property.Generic
  ( Property (..),
  )
where

import Data.Kind (Type)
import Data.List.NonEmpty (NonEmpty)
import Data.Record.Anon
import Data.Record.Anon.Simple (Record, empty, get, insert, project)
import Data.Type.Bool (If)
import GHC.OverloadedLabels
import GHC.TypeLits (Symbol)
import VCard.CaseInsensitive (CaseInsensitiveUpper (..))
import VCard.Types.Param.Generic (Param (..))
import VCard.Types.Param.ParamValue (ParamValue)
import VCard.Types.Param.Pref qualified as Param
import VCard.Types.Param.Value qualified as Param
import VCard.XName (IsXNameSymbol)

data Property (name :: Symbol) (xparams_allowed :: XParamInclusion) (params :: Row Type) (value :: Type) = Property
  { propertyName :: CaseInsensitiveUpper name,
    propertyParams :: PropParams xparams_allowed params,
    propertyValue :: value
  }

data XParamInclusion = XParamsAllowed | NoXParamsAllowed

data PropParams (xparams_allowed :: XParamInclusion) (standard_params :: Row Type) where
  WithXParams :: PermutedRecord ys -> PropParams XParamsAllowed (FilterNonXParams ys)
  NoXParams ::
    (FilterXParams ys ~ '[]) => PermutedRecord ys -> PropParams NoXParamsAllowed ys

data PermutedRecord (xs :: Row Type) where
  PermutedRecord :: (PermutedRow xs ys) => Record ys -> PermutedRecord xs

getP :: forall r r' n a. (RowHasField n r' a, SubRow r r') => Field n -> Record r -> a
getP field rec =
  let rec' :: Record r'
      rec' = project rec
   in get field rec'

instance (AllFields xs Show, KnownFields xs) => Show (PermutedRecord xs) where
  show (PermutedRecord (rec :: Record ys)) =
    "Permuted (" <> show ((project :: Record ys -> Record xs) rec) <> ")"

type family FilterNonXParams (params :: Row k) :: Row k where
  FilterNonXParams '[] = '[]
  FilterNonXParams ((name := value) : xs) =
    If (IsXNameSymbol name) (FilterNonXParams xs) ((name := value) : FilterNonXParams xs)

type family FilterXParams (params :: Row k) :: Row k where
  FilterXParams '[] = '[]
  FilterXParams ((name := value) : xs) =
    If
      (IsXNameSymbol name)
      ((name := value) : FilterXParams xs)
      (FilterXParams xs)

type SourceRow :: Row Type
type SourceRow =
  [ "VALUE" := Param.Value "uri",
    "PREF" := Param.Pref,
    "X-FOO" := Param "X-FOO" (NonEmpty ParamValue),
    "X-BAR" := Param "X-BAR" (NonEmpty ParamValue)
  ]

r :: Record ["Name" := String, "Age" := Word]
r =
  insert (fromLabel @"Name") "Preetham" $
    insert (fromLabel @"Age") 28 $
      empty

type family ParamsToRows (params :: [Type]) where
  ParamsToRows '[] = '[]
  ParamsToRows (Param name value : xs) = (name := value) : ParamsToRows xs

type PermutedRow xs ys = (SubRow xs ys, SubRow ys xs)
