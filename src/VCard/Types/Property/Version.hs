-- SPDX-FileCopyrightText: Copyright Preetham Gujjula
-- SPDX-License-Identifier: BSD-3-Clause
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE OverloadedStrings #-}

module VCard.Types.Property.Version
  ( Version (..),
  )
where

import Control.Monad (void)
import Data.Text (pack)
import Text.Megaparsec.Char (string)
import VCard.Parse (HasParser (..), Parser)
import VCard.Serialize (HasSerializer (..), Serializer)

-- | The version of the vCard specification that the VCard conforms to.
--   Currently only Version 4.0, specified in RFC 6350, is supported.
data Version = Version_4_0
  deriving (Eq, Show, Ord)

instance HasParser Version where
  parser :: Parser Version
  parser = do
    void (string "VERSION:4.0")
    pure Version_4_0

instance HasSerializer Version where
  serializer :: Serializer Version
  serializer = const (pack "VERSION:4.0")
