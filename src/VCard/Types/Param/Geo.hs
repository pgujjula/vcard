-- SPDX-FileCopyrightText: Copyright Preetham Gujjula
-- SPDX-License-Identifier: BSD-3-Clause
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE TypeApplications #-}

module VCard.Types.Param.Geo
  ( Geo,
    GeoValue (..),
  )
where

import Control.Monad (void)
import Text.Megaparsec.Char (string)
import VCard.Parse (HasParser, Parser, parser)
import VCard.Serialize (HasSerializer, Serializer, serializer)
import VCard.Types.Param.Generic (Param)
import VCard.Types.Value.URI (URI)
import VCard.Util (dquote)

type Geo = Param "GEO" GeoValue

newtype GeoValue = GeoValue {unGeoValue :: URI}
  deriving (Eq, Show, Ord)

instance HasParser GeoValue where
  parser :: Parser GeoValue
  parser = do
    void (string dquote)
    uri <- parser @URI
    void (string dquote)
    pure (GeoValue uri)

instance HasSerializer GeoValue where
  serializer :: Serializer GeoValue
  serializer (GeoValue uri) = dquote <> serializer uri <> dquote
