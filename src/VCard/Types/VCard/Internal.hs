-- SPDX-FileCopyrightText: Copyright Preetham Gujjula
-- SPDX-License-Identifier: BSD-3-Clause
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE OverloadedStrings #-}

module VCard.Types.VCard.Internal
  ( VCardEntity (..),
    VCard (..),
    version,
  )
where

import Control.Monad (void)
import Data.Function ((&))
import Data.Text (pack)
import Data.Text qualified as Text
import Text.Megaparsec (some)
import Text.Megaparsec.Char (string)
import VCard.Parse (HasParser (..), Parser, parser)
import VCard.Serialize (HasSerializer (..), Serializer, serialize, serializer)
import VCard.Types.Property.Version (Version (..))
import VCard.Util (crlf)

-- | Represents a collection of vCards.
newtype VCardEntity = VCardEntity {unVCardEntity :: [VCard]}
  deriving (Eq, Show, Ord)

instance HasParser VCardEntity where
  parser :: Parser VCardEntity
  parser = VCardEntity <$> some parser

instance HasSerializer VCardEntity where
  serializer :: Serializer VCardEntity
  serializer vCardEntity =
    unVCardEntity vCardEntity
      & map serializer
      & Text.concat

-- | Represents a single contact.
newtype VCard = VCard
  { vCardVersion :: Version
  }
  deriving (Eq, Show, Ord)

instance HasParser VCard where
  parser :: Parser VCard
  parser = do
    void (string ("BEGIN:VCARD" <> crlf))
    v <- parser <* string crlf
    void (string ("END:VCARD" <> crlf))
    pure $
      VCard
        { vCardVersion = v
        }

instance HasSerializer VCard where
  serializer :: Serializer VCard
  serializer vCard =
    Text.concat $
      map
        (<> crlf)
        [ pack "BEGIN:VCARD",
          serialize (vCardVersion vCard),
          pack "END:VCARD"
        ]

-- | The version of the vCard specification that the 'VCard' conforms to.
-- Currently only Version 4.0, specified in RFC 6350, is supported.
version :: VCard -> Version
version = vCardVersion
