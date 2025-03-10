-- SPDX-FileCopyrightText: Copyright Preetham Gujjula
-- SPDX-License-Identifier: BSD-3-Clause

module VCard.Types.Param.Value
  ( Value,
    ValueValue (..),
  )
where

import Data.Constraint (Dict (..))
import GHC.TypeLits (KnownSymbol, Symbol)
import VCard.CaseInsensitive (CaseInsensitiveLower)
import VCard.Parse (HasParser (..), Parser)
import VCard.Serialize (HasSerializer (..), Serializer)
import VCard.Symbol.Private (symbolSing)
import VCard.Types.Param.Generic (Param, mkParamParser, mkParamSerializer)
import VCard.Types.Param.Value.ValueValueSymbol
  ( ValueValueSymbol,
    testValueValueSymbol,
  )

type Value s = Param "VALUE" (ValueValue s)

data ValueValue (s :: Symbol) where
  ValueValue :: (ValueValueSymbol s) => CaseInsensitiveLower s -> ValueValue s

deriving instance Eq (ValueValue s)

deriving instance Show (ValueValue s)

instance (KnownSymbol s) => HasParser (Param "VALUE" (ValueValue s)) where
  parser = mkParamParser (parser @(ValueValue s))

instance HasSerializer (Param "VALUE" (ValueValue s)) where
  serializer = mkParamSerializer (serializer @(ValueValue s))

instance (KnownSymbol s) => HasParser (ValueValue s) where
  parser :: Parser (ValueValue s)
  parser =
    case testValueValueSymbol (symbolSing @s) of
      Nothing -> fail "no ValueValueSymbol"
      Just Dict -> ValueValue <$> parser @(CaseInsensitiveLower s)

instance HasSerializer (ValueValue s) where
  serializer :: Serializer (ValueValue s)
  serializer (ValueValue x) = serializer x
