-- SPDX-FileCopyrightText: Copyright Preetham Gujjula
-- SPDX-License-Identifier: BSD-3-Clause

module VCard.Types.Param.Calscale
  ( Calscale,
    CalscaleValue (..),
  )
where

import Data.Constraint (Dict (..))
import GHC.TypeLits (KnownSymbol, Symbol)
import VCard.Parse (HasParser, Parser, parser)
import VCard.Serialize (HasSerializer, Serializer, serializer)
import VCard.Types.Param.Calscale.CalscaleValueSymbol
  ( CalscaleValueSymbol,
    testCalscaleValueSymbol,
  )
import VCard.Types.Param.Generic (Param, mkParamParser, mkParamSerializer)
import VCard.Types.Textual (CaseInsensitiveLower)
import VCard.Util.Symbol (symbolSing)

type Calscale s = Param "CALSCALE" (CalscaleValue s)

data CalscaleValue (s :: Symbol) where
  CalscaleValue ::
    (CalscaleValueSymbol s) =>
    CaseInsensitiveLower s ->
    CalscaleValue s

deriving instance Eq (CalscaleValue s)

deriving instance Show (CalscaleValue s)

instance (KnownSymbol s) => HasParser (Param "CALSCALE" (CalscaleValue s)) where
  parser = mkParamParser (parser @(CalscaleValue s))

instance HasSerializer (Param "CALSCALE" (CalscaleValue s)) where
  serializer = mkParamSerializer (serializer @(CalscaleValue s))

instance (KnownSymbol s) => HasParser (CalscaleValue s) where
  parser :: Parser (CalscaleValue s)
  parser =
    case testCalscaleValueSymbol (symbolSing @s) of
      Nothing -> fail "no CalscaleValueSymbol"
      Just Dict -> CalscaleValue <$> parser @(CaseInsensitiveLower s)

instance HasSerializer (CalscaleValue s) where
  serializer :: Serializer (CalscaleValue s)
  serializer (CalscaleValue x) = serializer x
