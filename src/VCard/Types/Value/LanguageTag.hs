-- SPDX-FileCopyrightText: Copyright Preetham Gujjula
-- SPDX-License-Identifier: BSD-3-Clause

module VCard.Types.Value.LanguageTag
  ( LanguageTag (..),
  )
where

import Data.Char (isAlphaNum, isAscii)
import Data.Text (Text)
import Text.Megaparsec (takeWhile1P)
import VCard.Parse (HasParser, Parser, parser)
import VCard.Serialize (HasSerializer, Serializer, serializer)
import Prelude hiding (Integer)

-- | /Reference:/ [@Language-Tag@]
--     (https://gist.github.com/pgujjula/af9bacba47664a58eea383a5ae44b10b#file-rfc6350-txt-L585)
newtype LanguageTag = LanguageTag
  { unLanguageTag :: Text
  }
  deriving (Eq, Ord, Show)

instance HasParser LanguageTag where
  parser :: Parser LanguageTag
  parser = LanguageTag <$> takeWhile1P Nothing isAlphaNumDash

isAlphaNumDash :: Char -> Bool
isAlphaNumDash c = (isAlphaNum c || c == '-') && isAscii c

instance HasSerializer LanguageTag where
  serializer :: Serializer LanguageTag
  serializer = unLanguageTag
