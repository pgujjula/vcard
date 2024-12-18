-- SPDX-FileCopyrightText: Copyright Preetham Gujjula
-- SPDX-License-Identifier: BSD-3-Clause

-- | Support for serializing VCards
module VCard.Serialize (Serializer, HasSerializer (..)) where

import Data.Text (Text)

type Serializer a = a -> Text

class HasSerializer a where
  serializer :: Serializer a
