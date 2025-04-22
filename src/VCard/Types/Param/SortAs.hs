-- SPDX-FileCopyrightText: Copyright Preetham Gujjula
-- SPDX-License-Identifier: BSD-3-Clause
{-# OPTIONS_GHC -Wno-orphans #-}

module VCard.Types.Param.SortAs
  ( SortAsParam,
  )
where

import Data.List.NonEmpty (NonEmpty)
import Text.Megaparsec.Char (char)
import VCard.Parse (HasParser, Parser, parser)
import VCard.Serialize (HasSerializer, Serializer, serializer)
import VCard.Types.Param.Generic (GenericParam, mkParamParser, mkParamSerializer)
import VCard.Types.Param.ParamValue (ParamValue)
import VCard.Util (intersperseCommaNE, sepByNonEmpty)

type SortAsParam = GenericParam "SORT-AS" (NonEmpty ParamValue)

instance HasParser SortAsParam where
  parser :: Parser SortAsParam
  parser = mkParamParser (sepByNonEmpty (parser @ParamValue) (char ','))

instance HasSerializer SortAsParam where
  serializer :: Serializer SortAsParam
  serializer = mkParamSerializer (intersperseCommaNE (serializer @ParamValue))
