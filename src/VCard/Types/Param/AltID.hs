-- SPDX-FileCopyrightText: Copyright Preetham Gujjula
-- SPDX-License-Identifier: BSD-3-Clause
{-# OPTIONS_GHC -Wno-orphans #-}

module VCard.Types.Param.AltID
  ( -- * Types
    AltID,
    SAltID (..),
    SomeAltID (..),

    -- * Conversion
    altIDVal,
    someAltIDVal,
  )
where

import Data.Type.Equality ((:~:) (Refl))
import GHC.TypeLits (KnownSymbol)
import VCard.Parse (HasParser, Parser, parser)
import VCard.Serialize (HasSerializer, Serializer, serializer)
import VCard.Types.Param.Generic (Param (..), mkParamParser, mkParamSerializer)
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

type AltID = Param "ALTID" ParamValue

data SAltID s where
  SAltID :: Param "ALTID" (SParamValue s) -> SAltID (UnquoteParamValueSymbol s)

instance Eq (SAltID s) where
  SAltID (param1 :: Param "ALTID" (SParamValue a))
    == SAltID (param2 :: Param "ALTID" (SParamValue b)) =
      let sa :: SSymbol a
          sa = case paramValue param1 of SParamValue ss -> ss

          sb :: SSymbol b
          sb = case paramValue param2 of SParamValue ss -> ss
       in case testSSymbolEquality sa sb of
            Nothing -> False
            Just Refl -> param1 == param2

deriving instance Show (SAltID s)

data SomeAltID where
  SomeAltID :: SAltID s -> SomeAltID

instance Eq SomeAltID where
  (SomeAltID saltid1) == (SomeAltID saltid2) =
    case (saltid1, saltid2) of
      ( SAltID (param1 :: Param "ALTID" (SParamValue x)),
        SAltID (param2 :: Param "ALTID" (SParamValue y))
        ) ->
          let sx :: SSymbol x
              sx = case paramValue param1 of SParamValue ss -> ss
              sy :: SSymbol y
              sy = case paramValue param2 of SParamValue ss -> ss
           in case testSSymbolEquality sx sy of
                Nothing -> False
                Just Refl -> param1 == param2

deriving instance Show SomeAltID

altIDVal :: SAltID s -> AltID
altIDVal (SAltID altID) =
  Param
    { paramName = paramName altID,
      paramValue = paramValueVal (paramValue altID)
    }

someAltIDVal :: AltID -> SomeAltID
someAltIDVal altID =
  case someParamValueVal (paramValue altID) of
    SomeParamValue spv ->
      SomeAltID . SAltID $
        Param
          { paramName = paramName altID,
            paramValue = spv
          }

instance HasParser AltID where
  parser :: Parser AltID
  parser = mkParamParser (parser @ParamValue)

instance (KnownSymbol s) => HasParser (SAltID s) where
  parser :: Parser (SAltID s)
  parser =
    parser @SomeAltID >>= \case
      SomeAltID sAltID@(SAltID param) ->
        let validAltID =
              testSSymbolEquality
                (sUnquoteSParamValue (paramValue param))
                (symbolSing @s)
         in case validAltID of
              Just Refl -> pure sAltID
              Nothing -> fail "parser @(SAltID s): no parse)"

instance HasParser SomeAltID where
  parser :: Parser SomeAltID
  parser = someAltIDVal <$> parser @AltID

instance HasSerializer AltID where
  serializer :: Serializer AltID
  serializer = mkParamSerializer (serializer @ParamValue)

instance HasSerializer (SAltID s) where
  serializer :: Serializer (SAltID s)
  serializer (SAltID param) = mkParamSerializer serializer param

instance HasSerializer SomeAltID where
  serializer :: Serializer SomeAltID
  serializer (SomeAltID altID) = serializer altID
