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
import Data.List.NonEmpty (NonEmpty)
import Data.Text (Text)
import Text.Megaparsec (takeWhileP)
import Text.Megaparsec.Char (string)
import VCard.Parse (HasParser (..), Parser)
import VCard.Util (crlf)

newtype VCardEntity = VCardEntity {unVCardEntity :: NonEmpty VCard}
  deriving (Eq, Show, Ord)

instance HasParser VCardEntity where
  parser :: Parser VCardEntity
  parser = VCardEntity <$> NonEmpty.some parser

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

data Version = Version_4_0
  deriving (Eq, Show, Ord)

instance HasParser Version where
  parser :: Parser Version
  parser = do
    void (string ("VERSION:4.0" <> crlf))
    pure Version_4_0

newtype FN = FN {unFN :: Text}
  deriving (Eq, Show, Ord)

instance HasParser FN where
  parser :: Parser FN
  parser = do
    void (string "FN:")
    fnText <- takeWhileP Nothing (/= '\r')
    void (string crlf)
    pure (FN fnText)
