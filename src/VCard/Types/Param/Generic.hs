-- SPDX-FileCopyrightText: Copyright Preetham Gujjula
-- SPDX-License-Identifier: BSD-3-Clause
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module VCard.Types.Param.Generic
  ( GenericParam (..),
    mkParamParser,
    mkParamSerializer,
  )
where

import Control.Monad (void)
import Data.Kind (Type)
import GHC.TypeLits (KnownSymbol, Symbol)
import Text.Megaparsec.Char (char)
import VCard.Parse (HasParser (..), Parser)
import VCard.Serialize (HasSerializer (..), Serializer)
import VCard.Types.Textual (CaseInsensitiveUpper (..))

-- | A generic type for parameters. The different vCard parameters are type
--   synonyms of this type.
data GenericParam (name :: Symbol) (value :: Type) = GenericParam
  { genericParamName :: CaseInsensitiveUpper name,
    genericParamValue :: value
  }
  deriving (Eq, Show)

mkParamParser ::
  forall name value.
  (KnownSymbol name) =>
  Parser value ->
  Parser (GenericParam name value)
mkParamParser valueParser = do
  genericParamName <- parser @(CaseInsensitiveUpper name)
  void (char '=')
  genericParamValue <- valueParser
  pure $ GenericParam {..}

mkParamSerializer :: Serializer value -> Serializer (GenericParam name value)
mkParamSerializer valueSerializer (GenericParam {..}) =
  serializer genericParamName <> "=" <> valueSerializer genericParamValue
