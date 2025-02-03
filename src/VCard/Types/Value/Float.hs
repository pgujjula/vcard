-- SPDX-FileCopyrightText: Copyright Preetham Gujjula
-- SPDX-License-Identifier: BSD-3-Clause
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module VCard.Types.Value.Float
  ( Float (..),
    FloatList,
    NaturalLeadingZeros (..),
  )
where

import Data.Text qualified as Text
import GHC.Exts (int2Word#, word2Int#)
import GHC.Int (Int (I#))
import GHC.Word (Word (W#))
import Numeric.Natural (Natural)
import Text.Megaparsec (optional)
import Text.Megaparsec.Char (char)
import TextShow (showt)
import VCard.Parse (HasParser, Parser, parser)
import VCard.Serialize (HasSerializer, Serializer, serializer)
import VCard.Types.Value.Integer (naturalP)
import VCard.Types.Value.List (List (..))
import VCard.Types.Value.Time (Sign (..))
import Prelude hiding (Float, Integer)

data Float = Float
  { floatSign :: !(Maybe Sign),
    floatWholePart :: !NaturalLeadingZeros,
    floatDecimalPart :: !(Maybe NaturalLeadingZeros)
  }
  deriving (Eq, Show, Ord)

data NaturalLeadingZeros = NaturalLeadingZeros
  { naturalLeadingZerosNumLeadingZeros :: !Word,
    naturalLeadingZerosValue :: !Natural
  }
  deriving (Eq, Show, Ord)

naturalLeadingZerosP :: Parser NaturalLeadingZeros
naturalLeadingZerosP = uncurry NaturalLeadingZeros <$> naturalP

naturalLeadingZerosS :: Serializer NaturalLeadingZeros
naturalLeadingZerosS naturalLeadingZeros =
  Text.replicate
    (word2Int $ naturalLeadingZerosNumLeadingZeros naturalLeadingZeros)
    "0"
    <> showt (naturalLeadingZerosValue naturalLeadingZeros)

instance HasParser Float where
  parser :: Parser Float
  parser = do
    sign <- optional (parser @Sign)
    wholePart <- naturalLeadingZerosP
    decimalPart <- optional (char '.' *> naturalLeadingZerosP)
    pure $
      Float
        { floatSign = sign,
          floatWholePart = wholePart,
          floatDecimalPart = decimalPart
        }

instance HasSerializer Float where
  serializer :: Serializer Float
  serializer float =
    maybe "" serializer (floatSign float)
      <> naturalLeadingZerosS (floatWholePart float)
      <> maybe "" (("." <>) . naturalLeadingZerosS) (floatDecimalPart float)

type FloatList = List Float

-- Utilities
int2Word :: Int -> Word
int2Word (I# i) = W# (int2Word# i)
{-# INLINE int2Word #-}

word2Int :: Word -> Int
word2Int (W# w) = I# (word2Int# w)
{-# INLINE word2Int #-}
