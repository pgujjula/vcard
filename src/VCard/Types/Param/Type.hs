-- SPDX-FileCopyrightText: Copyright Preetham Gujjula
-- SPDX-License-Identifier: BSD-3-Clause

module VCard.Types.Param.Type
  ( Type,
    TypeGeneral,
    TypeGeneralSymbol,
    TypeTel,
    TypeTelSymbol,
    TypeRelated,
    TypeRelatedSymbol,
    TypeValue (..),
  )
where

import Data.Char (isAlpha, isAscii, isDigit)
import Data.Constraint (Dict (..))
import Data.Kind (Constraint)
import Data.List.NonEmpty (NonEmpty (..))
import Data.Maybe (isJust)
import Data.Text (Text)
import Data.Text qualified as Text
import GHC.TypeLits (Symbol)
import Text.Megaparsec (takeWhile1P)
import Text.Megaparsec.Char (char)
import VCard.Parse (HasParser, Parser, parser)
import VCard.Serialize (HasSerializer, Serializer, serializer)
import VCard.Types.Param.Generic (GenericParam, mkParamParser, mkParamSerializer)
import VCard.Types.Param.Type.TypeValueSymbol
  ( TypeGeneralSymbol,
    TypeRelatedSymbol,
    TypeTelSymbol,
    testTypeGeneralSymbol,
    testTypeRelatedSymbol,
    testTypeTelSymbol,
  )
import VCard.Types.Textual (CaseInsensitiveLower (..))
import VCard.Util (intersperseCommaNE, sepByNonEmpty)
import VCard.Util.Symbol
  ( SSymbol,
    sToLower,
    testSSymbolEquality,
    withSomeSSymbol,
  )

type Type symbol_class = GenericParam "TYPE" (NonEmpty (TypeValue symbol_class))

type TypeGeneral = Type TypeGeneralSymbol

type TypeRelated = Type TypeRelatedSymbol

type TypeTel = Type TypeTelSymbol

data TypeValue (symbol_class :: Symbol -> Constraint) where
  TypeValue ::
    (symbol_class s) => CaseInsensitiveLower s -> TypeValue symbol_class

instance Eq (TypeValue symbol_class) where
  (==)
    (TypeValue (CaseInsensitiveLower s1))
    (TypeValue (CaseInsensitiveLower s2)) = isJust (testSSymbolEquality s1 s2)

deriving instance Show (TypeValue symbol_class)

mkTypeValueParser ::
  (forall s. SSymbol s -> Maybe (Dict (symbol_class s))) ->
  Parser (TypeValue symbol_class)
mkTypeValueParser tester = do
  (name :: Text) <- takeWhile1P (Just "type name") isTokenChar
  withSomeSSymbol (Text.unpack name) $ \(sname :: SSymbol name) ->
    case tester (sToLower sname) of
      Nothing -> fail "TypeValue TypeGeneralSymbol: no parse"
      Just Dict -> pure $ TypeValue (CaseInsensitiveLower sname)

instance HasParser (GenericParam "TYPE" (NonEmpty (TypeValue TypeGeneralSymbol))) where
  parser :: Parser (GenericParam "TYPE" (NonEmpty (TypeValue TypeGeneralSymbol)))
  parser =
    mkParamParser
      (sepByNonEmpty (parser @(TypeValue TypeGeneralSymbol)) (char ','))

instance
  HasSerializer
    (GenericParam "TYPE" (NonEmpty (TypeValue TypeGeneralSymbol)))
  where
  serializer ::
    Serializer (GenericParam "TYPE" (NonEmpty (TypeValue TypeGeneralSymbol)))
  serializer =
    mkParamSerializer $
      intersperseCommaNE (serializer @(TypeValue TypeGeneralSymbol))

instance HasParser (GenericParam "TYPE" (NonEmpty (TypeValue TypeTelSymbol))) where
  parser :: Parser (GenericParam "TYPE" (NonEmpty (TypeValue TypeTelSymbol)))
  parser =
    mkParamParser
      (sepByNonEmpty (parser @(TypeValue TypeTelSymbol)) (char ','))

instance HasSerializer (GenericParam "TYPE" (NonEmpty (TypeValue TypeTelSymbol))) where
  serializer :: Serializer (GenericParam "TYPE" (NonEmpty (TypeValue TypeTelSymbol)))
  serializer =
    mkParamSerializer $
      intersperseCommaNE (serializer @(TypeValue TypeTelSymbol))

instance HasParser (GenericParam "TYPE" (NonEmpty (TypeValue TypeRelatedSymbol))) where
  parser :: Parser (GenericParam "TYPE" (NonEmpty (TypeValue TypeRelatedSymbol)))
  parser =
    mkParamParser
      (sepByNonEmpty (parser @(TypeValue TypeRelatedSymbol)) (char ','))

instance
  HasSerializer
    (GenericParam "TYPE" (NonEmpty (TypeValue TypeRelatedSymbol)))
  where
  serializer ::
    Serializer (GenericParam "TYPE" (NonEmpty (TypeValue TypeRelatedSymbol)))
  serializer =
    mkParamSerializer $
      intersperseCommaNE (serializer @(TypeValue TypeRelatedSymbol))

instance HasParser (TypeValue TypeGeneralSymbol) where
  parser :: Parser (TypeValue TypeGeneralSymbol)
  parser = mkTypeValueParser testTypeGeneralSymbol

instance HasParser (TypeValue TypeTelSymbol) where
  parser :: Parser (TypeValue TypeTelSymbol)
  parser = mkTypeValueParser testTypeTelSymbol

instance HasParser (TypeValue TypeRelatedSymbol) where
  parser :: Parser (TypeValue TypeRelatedSymbol)
  parser = mkTypeValueParser testTypeRelatedSymbol

instance HasSerializer (TypeValue symbol_class) where
  serializer :: Serializer (TypeValue symbol_class)
  serializer (TypeValue x) = serializer x

-- Utilities
isTokenChar :: Char -> Bool
isTokenChar c =
  (isAlpha c && isAscii c)
    || isDigit c
    || c == '-'
