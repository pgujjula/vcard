-- SPDX-FileCopyrightText: Copyright Preetham Gujjula
-- SPDX-License-Identifier: BSD-3-Clause
{-# LANGUAGE InstanceSigs #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module VCard.Types.Value.LanguageTag
  ( LanguageTag,
  )
where

import Data.BCP47 (BCP47)
import Data.BCP47 qualified as BCP47
import VCard.Parse (HasParser, Parser, parser)
import VCard.Serialize (HasSerializer, Serializer, serializer)
import Prelude hiding (Integer)

type LanguageTag = BCP47

instance HasParser BCP47 where
  parser :: Parser BCP47
  parser = BCP47.parser

instance HasSerializer BCP47 where
  serializer :: Serializer BCP47
  serializer = BCP47.toText
