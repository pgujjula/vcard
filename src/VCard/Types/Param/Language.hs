-- SPDX-FileCopyrightText: Copyright Preetham Gujjula
-- SPDX-License-Identifier: BSD-3-Clause
{-# OPTIONS_GHC -Wno-orphans #-}

module VCard.Types.Param.Language
  ( LanguageParam,
  )
where

import VCard.Parse (HasParser, parser)
import VCard.Serialize (HasSerializer, serializer)
import VCard.Types.Param.Generic (GenericParam, mkParamParser, mkParamSerializer)
import VCard.Types.Value.LanguageTag (LanguageTag)

type LanguageParam = GenericParam "LANGUAGE" LanguageTag

instance HasParser (GenericParam "LANGUAGE" LanguageTag) where
  parser = mkParamParser (parser @LanguageTag)

instance HasSerializer (GenericParam "LANGUAGE" LanguageTag) where
  serializer = mkParamSerializer (serializer @LanguageTag)
