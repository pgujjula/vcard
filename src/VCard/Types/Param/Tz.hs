-- SPDX-FileCopyrightText: Copyright Preetham Gujjula
-- SPDX-License-Identifier: BSD-3-Clause

module VCard.Types.Param.Tz
  ( TzParam,
    TzValue (..),
  )
where

import Data.Text qualified as Text
import Text.Megaparsec (choice, try)
import Text.Megaparsec.Char (char)
import VCard.Char (dQuote)
import VCard.Parse (HasParser, Parser, parser)
import VCard.Serialize (HasSerializer, Serializer, serializer)
import VCard.Types.Param.Generic (GenericParam, mkParamParser, mkParamSerializer)
import VCard.Types.Param.ParamValue (ParamValue)
import VCard.Types.Value.URI (URI)
import Vary (Vary)
import Vary qualified (exhaustiveCase, from, on)

type TzParam = GenericParam "TZ" TzValue

newtype TzValue = TzValue {unTzValue :: Vary '[ParamValue, URI]}
  deriving (Eq, Show)

instance HasParser TzValue where
  parser :: Parser TzValue
  parser =
    TzValue
      <$> choice
        [ try (Vary.from <$> (char dQuote *> parser @URI <* char dQuote)),
          Vary.from <$> parser @ParamValue
        ]

instance HasParser TzParam where
  parser :: Parser TzParam
  parser = mkParamParser (parser @TzValue)

instance HasSerializer TzValue where
  serializer :: Serializer TzValue
  serializer =
    ( Vary.on (\(paramValue :: ParamValue) -> serializer paramValue)
        . Vary.on
          ( \(uri :: URI) ->
              Text.singleton dQuote <> serializer uri <> Text.singleton dQuote
          )
        $ Vary.exhaustiveCase
    )
      . unTzValue

instance HasSerializer TzParam where
  serializer :: Serializer TzParam
  serializer = mkParamSerializer (serializer @TzValue)
