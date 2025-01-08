-- SPDX-FileCopyrightText: Copyright Preetham Gujjula
-- SPDX-License-Identifier: BSD-3-Clause
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE OverloadedStrings #-}

module VCard.Types.Value.List
  ( List (..),
  )
where

import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.List.NonEmpty qualified as NonEmpty
import Data.Text qualified
import Text.Megaparsec (many)
import Text.Megaparsec.Char (char)
import VCard.Parse (HasParser, Parser, parser)
import VCard.Serialize (HasSerializer, serializer)

-- | Lists of values, separated by a comma. Defined in the front matter of
-- Section 4 of RFC 6350. Not to be confused with 'Data.List.List' from
-- "Data.List".
newtype List a = List {unList :: NonEmpty a}
  deriving (Eq, Show, Ord)

instance (HasParser a) => HasParser (List a) where
  parser :: Parser (List a)
  parser = do
    x <- parser
    xs <- many (char ',' *> parser)
    pure (List (x :| xs))

instance (HasSerializer a) => HasSerializer (List a) where
  serializer :: List a -> Data.Text.Text
  serializer (List xs) =
    Data.Text.intercalate "," $
      map serializer (NonEmpty.toList xs)
