-- SPDX-FileCopyrightText: Copyright Preetham Gujjula
-- SPDX-License-Identifier: BSD-3-Clause
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module VCard.Types
  ( VCard (..),
    VCardEntity (..),
    FN (..),
    Version (..),
    ValueParam (..),
    ValueType (..),
    XName (..),
  )
where

import Control.Monad (void)
import Control.Monad.Combinators (choice, (<|>))
import Control.Monad.Combinators.NonEmpty qualified as NonEmpty
import Data.Char (isAlphaNum, isAscii)
import Data.Function ((&))
import Data.Functor (($>))
import Data.List.NonEmpty (NonEmpty)
import Data.List.NonEmpty qualified as NonEmpty
import Data.Text (Text, pack)
import Data.Text qualified as Text
import Text.Megaparsec (takeWhile1P, takeWhileP)
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

newtype ValueParam = ValueParam {unValueParam :: ValueType}
  deriving (Eq, Show, Ord)

instance HasParser ValueParam where
  parser :: Parser ValueParam
  parser = do
    void (string "VALUE=")
    ValueParam <$> parser @ValueType

instance HasSerializer ValueParam where
  serializer :: Serializer ValueParam
  serializer (ValueParam valueType) =
    "VALUE=" <> serializer valueType

data ValueType
  = VTText
  | VTURI
  | VTDate
  | VTTime
  | VTDateTime
  | VTDateAndOrTime
  | VTTimestamp
  | VTBoolean
  | VTInteger
  | VTFloat
  | VTUTCOffset
  | VTLanguageTag
  | VTXName XName
  deriving (Eq, Show, Ord)

instance HasParser ValueType where
  parser :: Parser ValueType
  parser =
    choice
      [ string "text" $> VTText,
        string "uri" $> VTURI,
        string "date" $> VTDate,
        string "time" $> VTTime,
        string "date-time" $> VTDateTime,
        string "date-and-or-time" $> VTDateAndOrTime,
        string "timestamp" $> VTTimestamp,
        string "boolean" $> VTBoolean,
        string "integer" $> VTInteger,
        string "float" $> VTFloat,
        string "utc-offset" $> VTUTCOffset,
        string "language-tag" $> VTLanguageTag,
        VTXName <$> parser
      ]

instance HasSerializer ValueType where
  serializer :: Serializer ValueType
  serializer = \case
    VTText -> "text"
    VTURI -> "uri"
    VTDate -> "date"
    VTTime -> "time"
    VTDateTime -> "date-time"
    VTDateAndOrTime -> "date-and-or-time"
    VTTimestamp -> "timestamp"
    VTBoolean -> "boolean"
    VTInteger -> "integer"
    VTFloat -> "float"
    VTUTCOffset -> "utc-offset"
    VTLanguageTag -> "language-tag"
    VTXName xName -> serializer xName

newtype XName = XName {unXName :: Text}
  deriving (Eq, Show, Ord)

instance HasParser XName where
  parser :: Parser XName
  parser = do
    void (string "x-" <|> string "X-")
    xNameText <-
      takeWhile1P Nothing $ \c ->
        (isAscii c && isAlphaNum c) || c == '-'
    pure (XName xNameText)

instance HasSerializer XName where
  serializer :: Serializer XName
  serializer xName = "x-" <> unXName xName
