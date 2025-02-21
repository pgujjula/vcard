-- SPDX-FileCopyrightText: Copyright Preetham Gujjula
-- SPDX-License-Identifier: BSD-3-Clause
{-# LANGUAGE DataKinds #-}

module VCard.Types.Param.Language
  ( Language,
  )
where

import VCard.Types.Param.Generic (Param)
import VCard.Types.Value.LanguageTag (LanguageTag)

type Language = Param "LANGUAGE" LanguageTag
