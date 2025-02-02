-- SPDX-FileCopyrightText: Copyright Preetham Gujjula
-- SPDX-License-Identifier: BSD-3-Clause
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

module VCard.Types.Value.Integer
  ( Integer (..),
    IntegerList,
    IntegerValue (..),
  )
where

import Control.Monad (void)
import Data.Char (isDigit)
import Data.Finite (Finite, getFinite, packFinite)
import Data.Proxy (Proxy (..))
import Data.Text qualified as Text
import Data.Text.Read qualified as Text (decimal)
import Data.Type.Natural (KnownNat, natVal, type (+), type (^))
import GHC.Exts (int2Word#, word2Int#)
import GHC.Int (Int (I#))
import GHC.Word (Word (W#))
import Numeric.Natural (Natural)
import Text.Megaparsec (choice, takeWhile1P)
import Text.Megaparsec.Char (char)
import TextShow (showt)
import VCard.Parse (HasParser, Parser, parser)
import VCard.Serialize (HasSerializer, Serializer, serializer)
import VCard.Types.Value.List (List (..))
import Prelude hiding (Integer, fromInteger)

data Integer = Integer
  { integerNumLeadingZeros :: !Word,
    integerIntegerValue :: !IntegerValue
  }
  deriving (Eq, Show, Ord)

data IntegerValue
  = Unsigned !(Finite (2 ^ 63))
  | Positive !(Finite (2 ^ 63))
  | Negative !(Finite (2 ^ 63 + 1))
  deriving (Eq, Show, Ord)

instance HasParser Integer where
  parser :: Parser Integer
  parser = choice [unsignedPositiveP, signedPositiveP, signedNegativeP]

unsignedPositiveP :: Parser Integer
unsignedPositiveP = do
  (numLeadingZeros, fin) <- finiteP
  pure $
    Integer
      { integerNumLeadingZeros = numLeadingZeros,
        integerIntegerValue = Unsigned fin
      }

signedPositiveP :: Parser Integer
signedPositiveP = do
  void (char '+')
  (numLeadingZeros, fin) <- finiteP
  pure $
    Integer
      { integerNumLeadingZeros = numLeadingZeros,
        integerIntegerValue = Positive fin
      }

signedNegativeP :: Parser Integer
signedNegativeP = do
  void (char '-')
  (numLeadingZeros, fin) <- finiteP
  pure $
    Integer
      { integerNumLeadingZeros = numLeadingZeros,
        integerIntegerValue = Negative fin
      }

finiteP :: forall n. (KnownNat n) => Parser (Word, Finite n)
finiteP = do
  (numLeadingZeros, value) <- parseLeadingZeros
  let integer = toInteger value
  case packFinite integer of
    Nothing ->
      let errorMessage :: String
          errorMessage =
            "could not pack Integer ("
              <> show integer
              <> ") into Finite "
              <> show (natVal (Proxy :: Proxy n))
       in fail errorMessage
    Just x -> pure (numLeadingZeros, x)

parseLeadingZeros :: Parser (Word, Natural)
parseLeadingZeros = do
  digits <- takeWhile1P (Just "digits") isDigit
  let numLeadingZeros =
        if Text.all (== '0') digits
          then Text.length digits - 1
          else Text.length (Text.takeWhile (== '0') digits)
  case Text.decimal digits of
    Left errorMessage -> fail errorMessage
    Right (x, _) -> pure (int2Word numLeadingZeros, x)

instance HasSerializer Integer where
  serializer :: Serializer Integer
  serializer integer = sign <> zeros <> value
    where
      sign = case integerIntegerValue integer of
        Unsigned _ -> ""
        Positive _ -> "+"
        Negative _ -> "-"
      zeros = Text.replicate (word2Int (integerNumLeadingZeros integer)) "0"
      value = showt $ case integerIntegerValue integer of
        Unsigned x -> getFinite x
        Positive x -> getFinite x
        Negative x -> getFinite x

type IntegerList = List Integer

-- Utilities
int2Word :: Int -> Word
int2Word (I# i) = W# (int2Word# i)
{-# INLINE int2Word #-}

word2Int :: Word -> Int
word2Int (W# w) = I# (word2Int# w)
{-# INLINE word2Int #-}
