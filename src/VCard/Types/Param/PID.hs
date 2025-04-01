-- SPDX-FileCopyrightText: Copyright Preetham Gujjula
-- SPDX-License-Identifier: BSD-3-Clause
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module VCard.Types.Param.PID
  ( PID,
    PIDValue (..),
    Digit,
  )
where

import Control.Monad.Combinators.NonEmpty qualified as NonEmpty
import Data.Finite (Finite, getFinite)
import Data.Functor (($>))
import Data.List.NonEmpty (NonEmpty (..))
import Data.List.NonEmpty qualified as NonEmpty
import Data.Text qualified as Text
import Text.Megaparsec (choice, optional)
import Text.Megaparsec.Char (char)
import VCard.Parse (HasParser, Parser, parser)
import VCard.Serialize (HasSerializer, Serializer, serializer)
import VCard.Types.Param.Generic (GenericParam, mkParamParser, mkParamSerializer)
import VCard.Util (intToText, intersperseCommaNE, sepByNonEmpty)

type PID = GenericParam "PID" (NonEmpty PIDValue)

type Digit = Finite 10

data PIDValue = PIDValue
  { pidValueWholePart :: NonEmpty Digit,
    pidValueDecimalPart :: Maybe (NonEmpty Digit)
  }
  deriving (Eq, Show, Ord)

instance HasParser PID where
  parser :: Parser PID
  parser = mkParamParser (sepByNonEmpty (parser @PIDValue) (char ','))

instance HasSerializer PID where
  serializer :: Serializer PID
  serializer = mkParamSerializer (intersperseCommaNE (serializer @PIDValue))

instance HasParser PIDValue where
  parser :: Parser PIDValue
  parser = do
    pidValueWholePart <- NonEmpty.some digitP
    pidValueDecimalPart <- optional (char '.' *> NonEmpty.some digitP)
    pure $ PIDValue {..}

instance HasSerializer PIDValue where
  serializer :: Serializer PIDValue
  serializer (PIDValue {..}) =
    digitsS pidValueWholePart
      <> maybe "" (("." <>) . digitsS) pidValueDecimalPart

digitP :: Parser Digit
digitP =
  choice
    [ char '0' $> 0,
      char '1' $> 1,
      char '2' $> 2,
      char '3' $> 3,
      char '4' $> 4,
      char '5' $> 5,
      char '6' $> 6,
      char '7' $> 7,
      char '8' $> 8,
      char '9' $> 9
    ]

digitS :: Serializer Digit
digitS = intToText . getFinite

digitsS :: Serializer (NonEmpty Digit)
digitsS = Text.concat . map digitS . NonEmpty.toList
