-- SPDX-FileCopyrightText: Copyright Preetham Gujjula
-- SPDX-License-Identifier: BSD-3-Clause

module VCard.Serialize (HasSerializer (..), Serializer, serialize) where

import Data.Text (Text)

-- A simple serialization type.
type Serializer a = a -> Text

-- | A class for components of a vCard that can be serialized. Most types in
-- this library implement this class.
class HasSerializer a where
  serializer :: Serializer a

serialize :: (HasSerializer a) => a -> Text
serialize = serializer
