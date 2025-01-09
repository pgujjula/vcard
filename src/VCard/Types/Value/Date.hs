-- SPDX-FileCopyrightText: Copyright Preetham Gujjula
-- SPDX-License-Identifier: BSD-3-Clause
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE InstanceSigs #-}

module VCard.Types.Value.Date
  ( Day (..),
  )
where

import Data.Char (ord)
import Data.Finite (Finite, getFinite, packFinite)
import Text.Megaparsec.Char (digitChar)
import TextShow (showt)
import VCard.Parse (HasParser, Parser, parser)
import VCard.Serialize (HasSerializer, Serializer, serializer)

-- | A day of the month.
newtype Day = Day {unDay :: Finite 31}
  deriving (Eq, Show, Ord)

instance HasParser Day where
  parser :: Parser Day
  parser = do
    d1 <- toDigit <$> digitChar
    d2 <- toDigit <$> digitChar
    let dayInt = 10 * d1 + d2
    case packFinite (toInteger dayInt - 1) of
      Just dayFinite -> pure (Day dayFinite)
      Nothing -> fail $ show dayInt <> " was out of bounds for day (01 to 31)"

instance HasSerializer Day where
  serializer :: Serializer Day
  serializer (Day day) =
    let dayInt = getFinite day + 1
        (d1, d2) = dayInt `quotRem` 10
     in showt d1 <> showt d2

-- Utilities
toDigit :: Char -> Int
toDigit c = ord c - ord '0'
