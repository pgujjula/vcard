-- SPDX-FileCopyrightText: Copyright Preetham Gujjula
-- SPDX-License-Identifier: BSD-3-Clause

-- |
-- Module     : VCard.Types.Property
-- Copyright  : Copyright Preetham Gujjula
-- License    : BSD-3-Clause
-- Maintainer : Preetham Gujjula <libraries@mail.preetham.io>
-- Stability  : experimental
--
-- Types for the properties described in Section 6 of RFC 6350.
module VCard.Types.Property
  ( -- GenericProperty (..),
--    BeginProperty (..),
--    EndProperty (..),
  )
where

import Data.Kind (Type)
import GHC.TypeLits (Symbol)
-- import VCard.Types.Property.ParamMap1 (ParamMap)
import VCard.Types.Textual (CaseInsensitiveUpper)

-- | A generic type for parameters. The different vCard parameters are type
--   synonyms of this type.
--data GenericProperty (name :: Symbol) (params :: [Type]) (value :: Type)
--  = GenericProperty
--  { genericPropertyName :: CaseInsensitiveUpper name,
--    genericPropertyParams :: ParamMap params,
--    genericPropertyValue :: value
--  }

--type BeginProperty = GenericProperty "BEGIN" '[] (CaseInsensitiveUpper "VCARD")
--
--type EndProperty = GenericProperty "END" '[] (CaseInsensitiveUpper "VCARD")
