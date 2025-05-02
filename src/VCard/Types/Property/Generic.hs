-- SPDX-FileCopyrightText: Copyright Preetham Gujjula
-- SPDX-License-Identifier: BSD-3-Clause

-- |
-- Module     : VCard.Types.Property.Generic
-- Copyright  : Copyright Preetham Gujjula
-- License    : BSD-3-Clause
-- Maintainer : Preetham Gujjula <libraries@mail.preetham.io>
-- Stability  : experimental
module VCard.Types.Property.Generic
  ( GenericProperty (..),
  )
where

import Data.Kind (Type)
import GHC.TypeLits (Symbol)
import VCard.Types.Property.ParamMap (ParamMap)
import VCard.Types.Textual (CaseInsensitiveUpper)

-- | A generic type for parameters. The different vCard parameters are type
--   synonyms of this type.
data GenericProperty (name :: Symbol) (params :: [Type]) (value :: Type)
  = GenericProperty
  { genericPropertyName :: CaseInsensitiveUpper name,
    genericPropertyParams :: ParamMap params,
    genericPropertyValue :: value
  }
