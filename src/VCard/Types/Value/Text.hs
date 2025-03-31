-- SPDX-FileCopyrightText: Copyright Preetham Gujjula
-- SPDX-License-Identifier: BSD-3-Clause
{-# LANGUAGE OverloadedStrings #-}

module VCard.Types.Value.Text
  ( Text (..),
    TextList,
  )
where

import Data.Char (isAscii, ord)
import Data.Function ((&))
import Data.Functor (($>))
import Data.Proxy (Proxy (..))
import Data.Text qualified
import Text.Megaparsec (choice, many, oneOf, satisfy, tokensToChunk)
import Text.Megaparsec.Char (string)
import VCard.Parse (HasParser, Parser, parser)
import VCard.Serialize (HasSerializer, serializer)
import VCard.Types.Value.List (List)

-- | Not to be confused with 'Data.Text.Text' from "Data.Text".
--
--   /Reference:/ [@text@](https://gist.github.com/pgujjula/af9bacba47664a58eea383a5ae44b10b#file-rfc6350-txt-L521)
newtype Text = Text {unText :: Data.Text.Text}
  deriving (Eq, Show, Ord)

-- | /Reference:/ [@text-list@](https://gist.github.com/pgujjula/af9bacba47664a58eea383a5ae44b10b#file-rfc6350-txt-L531)
type TextList = List Text

instance HasParser Text where
  parser :: Parser Text
  parser = fmap Text textParser

textParser :: Parser Data.Text.Text
textParser =
  fmap (tokensToChunk (Proxy @Data.Text.Text)) . many $
    choice
      [ string "\\\\" $> '\\',
        string "\\," $> ',',
        string "\\N" $> '\n',
        string "\\n" $> '\n',
        wspP,
        nonAsciiP,
        printableAsciiNoBackslashCommaP
      ]
  where
    wspP :: Parser Char
    wspP = oneOf [' ', '\t']

    nonAsciiP :: Parser Char
    nonAsciiP = satisfy (not . isAscii)

    printableAsciiNoBackslashCommaP :: Parser Char
    printableAsciiNoBackslashCommaP =
      satisfy (\c -> isAscii c && isPrintable c && c /= ',' && c /= '\\')

    isPrintable :: Char -> Bool
    isPrintable c =
      let x = ord c
       in 0x21 <= x && x <= 0x7e

instance HasSerializer Text where
  serializer (Text text) =
    Data.Text.unpack text
      & map serializeChar
      & Data.Text.concat

serializeChar :: Char -> Data.Text.Text
serializeChar = \case
  '\\' -> "\\\\"
  ',' -> "\\,"
  '\n' -> "\\n"
  c -> Data.Text.singleton c
