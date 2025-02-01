-- SPDX-FileCopyrightText: Copyright Preetham Gujjula
-- SPDX-License-Identifier: BSD-3-Clause
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module VCard.Types.Value.Boolean
  ( Boolean (..),
    mkBoolean,
  )
where

import Data.Text (Text)
import Text.Megaparsec (choice, parseMaybe)
import Text.Megaparsec.Char (string')
import VCard.Parse (HasParser, Parser, parser)
import VCard.Serialize (HasSerializer, Serializer, serializer)

data Boolean = Boolean
  { booleanValue :: Bool,
    booleanText :: Text
  }
  deriving (Eq, Ord, Show)

instance HasParser Boolean where
  parser :: Parser Boolean
  parser = choice [trueP, falseP]
    where
      trueP :: Parser Boolean
      trueP = do
        text <- string' "TRUE"
        pure $
          Boolean
            { booleanValue = True,
              booleanText = text
            }

      falseP :: Parser Boolean
      falseP = do
        text <- string' "FALSE"
        pure $
          Boolean
            { booleanValue = False,
              booleanText = text
            }

instance HasSerializer Boolean where
  serializer :: Serializer Boolean
  serializer = booleanText

mkBoolean :: Text -> Maybe Boolean
mkBoolean = parseMaybe (parser @Boolean)
