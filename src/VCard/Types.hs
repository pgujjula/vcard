-- SPDX-FileCopyrightText: Copyright Preetham Gujjula
-- SPDX-License-Identifier: BSD-3-Clause
{-# LANGUAGE OverloadedStrings #-}

module VCard.Types
  ( VCard (..),
    VCardEntity (..),
    FN (..),
    Version (..),
    vCardEntityParser,
  )
where

import Control.Monad (void)
import Control.Monad.Combinators.NonEmpty qualified as NonEmpty
import Data.List.NonEmpty (NonEmpty)
import Data.Text (Text)
import Text.Megaparsec (takeWhileP)
import Text.Megaparsec.Char (string)
import VCard.Parse (Parser)
import VCard.Util (crlf)

newtype VCardEntity = VCardEntity {unVCardEntity :: NonEmpty VCard}
  deriving (Eq, Show, Ord)

data VCard = VCard
  { vCardVersion :: Version,
    vCardFN :: FN
  }
  deriving (Eq, Show, Ord)

data Version = Version_4_0
  deriving (Eq, Show, Ord)

newtype FN = FN {unFN :: Text}
  deriving (Eq, Show, Ord)

vCardEntityParser :: Parser VCardEntity
vCardEntityParser = VCardEntity <$> NonEmpty.some vCardParser

vCardParser :: Parser VCard
vCardParser = do
  void (string ("BEGIN:VCARD" <> crlf))
  void (string ("VERSION:4.0" <> crlf))
  fn <- fnParser
  void (string ("END:VCARD" <> crlf))
  pure $
    VCard
      { vCardVersion = Version_4_0,
        vCardFN = fn
      }

fnParser :: Parser FN
fnParser = do
  void (string "FN:")
  fnText <- takeWhileP Nothing (/= '\r')
  void (string crlf)
  pure (FN fnText)
