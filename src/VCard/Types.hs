-- SPDX-FileCopyrightText: Copyright Preetham Gujjula
-- SPDX-License-Identifier: BSD-3-Clause
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE OverloadedStrings #-}

module VCard.Types
  ( VCard (..),
    VCardEntity (..),
    FN (..),
    Version (..),
  )
where

import Control.Monad (void)
import Control.Monad.Combinators.NonEmpty qualified as NonEmpty
import Data.Function ((&))
import Data.List.NonEmpty (NonEmpty)
import Data.List.NonEmpty qualified as NonEmpty
import Data.Text (Text, pack)
import Data.Text qualified as Text
import Text.Megaparsec (takeWhileP)
import Text.Megaparsec.Char (string)
import VCard.Parse (HasParser (..), Parser)
import VCard.Serialize (HasSerializer (..), Serializer)
import VCard.Util (crlf)

newtype VCardEntity = VCardEntity {unVCardEntity :: NonEmpty VCard}
  deriving (Eq, Show, Ord)

instance HasParser VCardEntity where
  parser :: Parser VCardEntity
  parser = VCardEntity <$> NonEmpty.some parser

instance HasSerializer VCardEntity where
  serializer :: Serializer VCardEntity
  serializer vCardEntity =
    unVCardEntity vCardEntity
      & NonEmpty.toList
      & map serializer
      & Text.concat

data VCard = VCard
  { vCardVersion :: Version,
    vCardFN :: FN
  }
  deriving (Eq, Show, Ord)

instance HasParser VCard where
  parser :: Parser VCard
  parser = do
    void (string ("BEGIN:VCARD" <> crlf))
    version <- parser
    fn <- parser
    void (string ("END:VCARD" <> crlf))
    pure $
      VCard
        { vCardVersion = version,
          vCardFN = fn
        }

instance HasSerializer VCard where
  serializer :: Serializer VCard
  serializer vCard =
    Text.concat $
      map
        (<> crlf)
        [ pack "BEGIN:VCARD",
          serializer Version_4_0,
          serializer (vCardFN vCard),
          pack "END:VCARD"
        ]

data Version = Version_4_0
  deriving (Eq, Show, Ord)

instance HasParser Version where
  parser :: Parser Version
  parser = do
    void (string ("VERSION:4.0" <> crlf))
    pure Version_4_0

instance HasSerializer Version where
  serializer :: Serializer Version
  serializer = const (pack "VERSION:4.0")

newtype FN = FN {unFN :: Text}
  deriving (Eq, Show, Ord)

instance HasParser FN where
  parser :: Parser FN
  parser = do
    void (string "FN:")
    fnText <- takeWhileP Nothing (/= '\r')
    void (string crlf)
    pure (FN fnText)

instance HasSerializer FN where
  serializer :: Serializer FN
  serializer fn = Text.pack "FN:" <> unFN fn
