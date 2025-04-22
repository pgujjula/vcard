-- SPDX-FileCopyrightText: Copyright Preetham Gujjula
-- SPDX-License-Identifier: BSD-3-Clause

module VCard.Types.Param.Any
  ( AnyParam (..),
  )
where

import Control.Monad (void)
import Data.Constraint (Dict (..))
import Data.List.NonEmpty (NonEmpty)
import Data.Type.Equality ((:~:) (Refl))
import Text.Megaparsec.Char (char)
import VCard.Parse (HasParser, Parser, parser)
import VCard.Serialize (HasSerializer, Serializer, serializer)
import VCard.Types.Param.Generic (GenericParam (..), mkParamSerializer)
import VCard.Types.Param.ParamValue
  ( ParamValue,
  )
import VCard.Types.Textual
  ( CaseInsensitiveUpper (..),
    SXName (..),
    SomeXName (..),
    XName,
    XNameUpperSymbol,
    someXNameVal,
    testXNameUpperSymbol,
  )
import VCard.Util (intersperseCommaNE, sepByNonEmpty)
import VCard.Util.Symbol
  ( SSymbol,
    ToUpper,
    fromSSymbol,
    sToUpper,
    testSSymbolEquality,
  )

-- | /Reference:/ [@any-param@]
--     (https://gist.github.com/pgujjula/af9bacba47664a58eea383a5ae44b10b#file-rfc6350-txt-L1224)
data AnyParam where
  AnyParam :: (XNameUpperSymbol xname) => GenericParam xname (NonEmpty ParamValue) -> AnyParam

instance Eq AnyParam where
  (==)
    (AnyParam (xparam1 :: GenericParam xname1 (NonEmpty ParamValue)))
    (AnyParam (xparam2 :: GenericParam xname2 (NonEmpty ParamValue))) =
      case (genericParamName xparam1, genericParamName xparam2) of
        (CaseInsensitiveUpper ss1, CaseInsensitiveUpper ss2) ->
          case testSSymbolEquality ss1 ss2 of
            Nothing -> False
            Just Refl -> xparam1 == xparam2

deriving instance Show AnyParam

instance HasParser AnyParam where
  parser :: Parser AnyParam
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
                pure . AnyParam $
                  GenericParam
                    { genericParamName = CaseInsensitiveUpper st,
                      genericParamValue = values
                    }

instance HasSerializer AnyParam where
  serializer :: Serializer AnyParam
  serializer (AnyParam xparam) =
    mkParamSerializer (intersperseCommaNE (serializer @ParamValue)) xparam
