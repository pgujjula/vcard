-- SPDX-FileCopyrightText: Copyright Preetham Gujjula
-- SPDX-License-Identifier: BSD-3-Clause
{-# OPTIONS_GHC -Wno-orphans #-}

module VCard.Types.Param.SortAs
  ( SortAs,
  )
where

import Data.List.NonEmpty (NonEmpty)
import Text.Megaparsec.Char (char)
import VCard.Parse (HasParser, Parser, parser)
import VCard.Serialize (HasSerializer, Serializer, serializer)
import VCard.Types.Param.Generic (GenericParam, mkParamParser, mkParamSerializer)
import VCard.Types.Param.ParamValue (ParamValue)
import VCard.Util (intersperseCommaNE, sepByNonEmpty)

type SortAs = GenericParam "SORT-AS" (NonEmpty ParamValue)

instance HasParser SortAs where
  parser :: Parser SortAs
  parser = mkParamParser (sepByNonEmpty (parser @ParamValue) (char ','))

instance HasSerializer SortAs where
  serializer :: Serializer SortAs
  serializer = mkParamSerializer (intersperseCommaNE (serializer @ParamValue))
