-- SPDX-FileCopyrightText: Copyright Preetham Gujjula
-- SPDX-License-Identifier: BSD-3-Clause
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module VCard.Types.Param.Language
  ( Language,
  )
where

import VCard.Parse (HasParser, parser)
import VCard.Serialize (HasSerializer, serializer)
import VCard.Types.Param.Generic (Param, mkParamParser, mkParamSerializer)
import VCard.Types.Value.LanguageTag (LanguageTag)

type Language = Param "LANGUAGE" LanguageTag

instance HasParser (Param "LANGUAGE" LanguageTag) where
  parser = mkParamParser (parser @LanguageTag)

instance HasSerializer (Param "LANGUAGE" LanguageTag) where
  serializer = mkParamSerializer (serializer @LanguageTag)
