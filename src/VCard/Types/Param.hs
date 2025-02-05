-- SPDX-FileCopyrightText: Copyright Preetham Gujjula
-- SPDX-License-Identifier: BSD-3-Clause
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

-- |
-- Module     : VCard.Types.Param
-- Copyright  : Copyright Preetham Gujjula
-- License    : BSD-3-Clause
-- Maintainer : Preetham Gujjula <libraries@mail.preetham.io>
-- Stability  : experimental
--
-- Types for the property parameters described in Section 5 of RFC 6350.
module VCard.Types.Param
  ( -- * Generic @Param@
    Param (..),

    -- * Section 5.1 @LANGUAGE@
    Language,

    -- * Section 5.2 @VALUE@
    Value,
    ValueValue (..),
    ValueValueSymbol,

    -- * Section 5.3 @PREF@
    Pref,

    -- * Section 5.4 @ALTID@
    AltID,

    -- * Section 5.5 @PID@
    PID,
    PIDValue (..),

    -- * Section 5.6 @TYPE@
    Type,
    TypeValue (..),
    TypeValueSymbol,

    -- * Section 5.7 @MEDIATYPE@

    --    MediaTypeParam,
    --

    -- * Section 5.8 @CALSCALE@
    Calscale,
    CalscaleValue (..),
    CalscaleValueSymbol,

    -- * Section 5.9 @SORT-AS@
    SortAs,

    -- * Section 5.10 @GEO@
    Geo,

    -- * Section 5.11 @TZ@
    TZ,
    TZValue (..),
  )
where

import Data.Finite (Finite)
import Data.Kind qualified as Kind (Type)
import Data.List.NonEmpty (NonEmpty)
import Data.Typeable (Typeable)
import GHC.TypeLits (KnownSymbol, SSymbol, Symbol)
import Network.URI (URI)
import Numeric.Natural (Natural)
import VCard.Internal.Symbol (ToUpper)
import VCard.Types.Param.Calscale
  ( CalscaleValueSymbol,
  )
import VCard.Types.Param.ParamValue (ParamValue (..), SomeParamValue)
import VCard.Types.Param.Type (TypeValueSymbol)
import VCard.Types.Param.Value (ValueValueSymbol)
import VCard.Types.Value (LanguageTag (..))
import Vary (Vary)

data Param (name_normalized :: Symbol) (value :: Kind.Type) where
  Param ::
    ( KnownSymbol name_normalized,
      ToUpper name ~ name_normalized,
      Typeable value
    ) =>
    { paramName :: SSymbol name,
      paramValue :: value
    } ->
    Param name_normalized value

-- Section 5.1 LANGUAGE
type Language = Param "LANGUAGE" LanguageTag

-- Section 5.2 VALUE
type Value s = Param "VALUE" (ValueValue s)

data ValueValue (s :: Symbol) where
  ValueValue :: (ValueValueSymbol s) => SSymbol s -> ValueValue s

-- Section 5.3 PREF
type Pref = Param "PREF" (Finite 100)

-- Section 5.4 ALTID
type AltID altid_value = Param "ALTID" (ParamValue altid_value)

type PID = Param "PID" PIDValue

data PIDValue = PIDValue
  { pidValueWholePart :: Natural, -- Maybe Word?
    pidValueDecimalPart :: Natural
  }
  deriving (Eq, Show, Ord)

type Type = Param "TYPE" TypeValue

data TypeValue where
  TypeValue :: (TypeValueSymbol s) => SSymbol s -> TypeValue

type Calscale = Param "CALSCALE" CalscaleValue

data CalscaleValue where
  CalscaleValue :: (CalscaleValueSymbol s) => SSymbol s -> CalscaleValue

type SortAs = Param "SORT-AS" (NonEmpty SomeParamValue)

type Geo = Param "GEO" URI

type TZ = Param "TZ" TZValue

newtype TZValue = TZValue
  {unTZValue :: Vary '[SomeParamValue, URI]}
