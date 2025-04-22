-- SPDX-FileCopyrightText: Copyright Preetham Gujjula
-- SPDX-License-Identifier: BSD-3-Clause
{-# OPTIONS_GHC -Wno-orphans #-}

module VCard.Types.Param.AltID
  ( -- * Types
    AltIDParam,
    SAltIDParam (..),
    SomeAltIDParam (..),

    -- * Conversion
    altIDVal,
    someAltIDVal,
  )
where

import Data.Type.Equality ((:~:) (Refl))
import GHC.TypeLits (KnownSymbol)
import VCard.Parse (HasParser, Parser, parser)
import VCard.Serialize (HasSerializer, Serializer, serializer)
import VCard.Types.Param.Generic (GenericParam (..), mkParamParser, mkParamSerializer)
import VCard.Types.Param.ParamValue
  ( ParamValue,
    SParamValue (..),
    SomeParamValue (..),
    UnquoteParamValueSymbol,
    paramValueVal,
    sUnquoteSParamValue,
    someParamValueVal,
  )
import VCard.Util.Symbol (SSymbol, symbolSing, testSSymbolEquality)

type AltIDParam = GenericParam "ALTID" ParamValue

data SAltIDParam s where
  SAltIDParam :: GenericParam "ALTID" (SParamValue s) -> SAltIDParam (UnquoteParamValueSymbol s)

instance Eq (SAltIDParam s) where
  SAltIDParam (param1 :: GenericParam "ALTID" (SParamValue a))
    == SAltIDParam (param2 :: GenericParam "ALTID" (SParamValue b)) =
      let sa :: SSymbol a
          sa = case genericParamValue param1 of SParamValue ss -> ss

          sb :: SSymbol b
          sb = case genericParamValue param2 of SParamValue ss -> ss
       in case testSSymbolEquality sa sb of
            Nothing -> False
            Just Refl -> param1 == param2

deriving instance Show (SAltIDParam s)

data SomeAltIDParam where
  SomeAltIDParam :: SAltIDParam s -> SomeAltIDParam

instance Eq SomeAltIDParam where
  (SomeAltIDParam saltid1) == (SomeAltIDParam saltid2) =
    case (saltid1, saltid2) of
      ( SAltIDParam (param1 :: GenericParam "ALTID" (SParamValue x)),
        SAltIDParam (param2 :: GenericParam "ALTID" (SParamValue y))
        ) ->
          let sx :: SSymbol x
              sx = case genericParamValue param1 of SParamValue ss -> ss
              sy :: SSymbol y
              sy = case genericParamValue param2 of SParamValue ss -> ss
           in case testSSymbolEquality sx sy of
                Nothing -> False
                Just Refl -> param1 == param2

deriving instance Show SomeAltIDParam

altIDVal :: SAltIDParam s -> AltIDParam
altIDVal (SAltIDParam altID) =
  GenericParam
    { genericParamName = genericParamName altID,
      genericParamValue = paramValueVal (genericParamValue altID)
    }

someAltIDVal :: AltIDParam -> SomeAltIDParam
someAltIDVal altID =
  case someParamValueVal (genericParamValue altID) of
    SomeParamValue spv ->
      SomeAltIDParam . SAltIDParam $
        GenericParam
          { genericParamName = genericParamName altID,
            genericParamValue = spv
          }

instance HasParser AltIDParam where
  parser :: Parser AltIDParam
  parser = mkParamParser (parser @ParamValue)

instance (KnownSymbol s) => HasParser (SAltIDParam s) where
  parser :: Parser (SAltIDParam s)
  parser =
    parser @SomeAltIDParam >>= \case
      SomeAltIDParam sAltIDParam@(SAltIDParam param) ->
        let validAltIDParam =
              testSSymbolEquality
                (sUnquoteSParamValue (genericParamValue param))
                (symbolSing @s)
         in case validAltIDParam of
              Just Refl -> pure sAltIDParam
              Nothing -> fail "parser @(SAltIDParam s): no parse)"

instance HasParser SomeAltIDParam where
  parser :: Parser SomeAltIDParam
  parser = someAltIDVal <$> parser @AltIDParam

instance HasSerializer AltIDParam where
  serializer :: Serializer AltIDParam
  serializer = mkParamSerializer (serializer @ParamValue)

instance HasSerializer (SAltIDParam s) where
  serializer :: Serializer (SAltIDParam s)
  serializer (SAltIDParam param) = mkParamSerializer serializer param

instance HasSerializer SomeAltIDParam where
  serializer :: Serializer SomeAltIDParam
  serializer (SomeAltIDParam altID) = serializer altID
