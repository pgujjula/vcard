-- SPDX-FileCopyrightText: Copyright Preetham Gujjula
-- SPDX-License-Identifier: BSD-3-Clause

module VCard.Types.Param.Any
  ( Any (..),
  )
where

import Control.Monad (void)
import Data.Constraint (Dict (..))
import Data.List.NonEmpty (NonEmpty)
import Data.Type.Equality ((:~:) (Refl))
import Text.Megaparsec.Char (char)
import VCard.CaseInsensitive (CaseInsensitiveUpper (..))
import VCard.Parse (HasParser, Parser, parser)
import VCard.Serialize (HasSerializer, Serializer, serializer)
import VCard.Symbol.Private
  ( SSymbol,
    ToUpper,
    fromSSymbol,
    sToUpper,
    testSSymbolEquality,
  )
import VCard.Types.Param.Generic (Param (..), mkParamSerializer)
import VCard.Types.Param.ParamValue
  ( ParamValue,
  )
import VCard.Types.Textual.Private.XName
  ( SXName (..),
    SomeXName (..),
    XName,
    XNameUpperSymbol,
    someXNameVal,
    testXNameUpperSymbol,
  )
import VCard.Util (intersperseCommaNE, sepByNonEmpty)

data Any where
  Any :: (XNameUpperSymbol xname) => Param xname (NonEmpty ParamValue) -> Any

instance Eq Any where
  (==)
    (Any (xparam1 :: Param xname1 (NonEmpty ParamValue)))
    (Any (xparam2 :: Param xname2 (NonEmpty ParamValue))) =
      case (paramName xparam1, paramName xparam2) of
        (CaseInsensitiveUpper ss1, CaseInsensitiveUpper ss2) ->
          case testSSymbolEquality ss1 ss2 of
            Nothing -> False
            Just Refl -> xparam1 == xparam2

deriving instance Show Any

instance HasParser Any where
  parser :: Parser Any
  parser = do
    xname <- parser @XName
    case someXNameVal xname of
      SomeXName (SXName (st :: SSymbol t)) ->
        let ss :: SSymbol (ToUpper t)
            ss = sToUpper st
         in case testXNameUpperSymbol ss of
              Nothing ->
                error $
                  "panic: parser @Any: No instance (XNameUpperSymbol "
                    <> fromSSymbol ss
                    <> ")"
              Just Dict -> do
                void (char '=')
                values <- sepByNonEmpty (parser @ParamValue) (char ',')
                pure . Any $
                  Param
                    { paramName = CaseInsensitiveUpper st,
                      paramValue = values
                    }

instance HasSerializer Any where
  serializer :: Serializer Any
  serializer (Any xparam) =
    mkParamSerializer (intersperseCommaNE (serializer @ParamValue)) xparam
