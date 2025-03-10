-- SPDX-FileCopyrightText: Copyright Preetham Gujjula
-- SPDX-License-Identifier: BSD-3-Clause
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module VCard.Types.Param.Generic
  ( Param (..),
    mkParamParser,
    mkParamSerializer,
  )
where

import Control.Monad (void)
import Data.Kind (Type)
import GHC.TypeLits (KnownSymbol, Symbol)
import Text.Megaparsec.Char (char)
import VCard.CaseInsensitive (CaseInsensitiveUpper (..))
import VCard.Parse (HasParser (..), Parser)
import VCard.Serialize (HasSerializer (..), Serializer)

-- | A generic type for parameters. The different vCard parameters are type
--   synonyms of this type.
data Param (name :: Symbol) (value :: Type) = Param
  { paramName :: CaseInsensitiveUpper name,
    paramValue :: value
  }
  deriving (Eq, Show)

mkParamParser ::
  forall name value.
  (KnownSymbol name) =>
  Parser value ->
  Parser (Param name value)
mkParamParser valueParser = do
  paramName <- parser @(CaseInsensitiveUpper name)
  void (char '=')
  paramValue <- valueParser
  pure $ Param {..}

mkParamSerializer :: Serializer value -> Serializer (Param name value)
mkParamSerializer valueSerializer (Param {..}) =
  serializer paramName <> "=" <> valueSerializer paramValue
