-- SPDX-FileCopyrightText: Copyright Preetham Gujjula
-- SPDX-License-Identifier: BSD-3-Clause
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}

module VCard.Types.Param.Generic (Param (..)) where

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

instance
  forall name value.
  (KnownSymbol name, HasParser value) =>
  HasParser (Param name value)
  where
  parser :: Parser (Param name value)
  parser = do
    paramName <- parser @(CaseInsensitiveUpper name)
    void (char '=')
    paramValue <- parser @value
    pure $ Param {..}

instance (HasSerializer value) => HasSerializer (Param name value) where
  serializer :: Serializer (Param name value)
  serializer (Param {..}) =
    serializer paramName <> "=" <> serializer paramValue
