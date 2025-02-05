-- SPDX-FileCopyrightText: Copyright Preetham Gujjula
-- SPDX-License-Identifier: BSD-3-Clause
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -fplugin=Data.Record.Anon.Plugin #-}

-- |
-- Module     : VCard.Types.Property
-- Copyright  : Copyright Preetham Gujjula
-- License    : BSD-3-Clause
-- Maintainer : Preetham Gujjula <libraries@mail.preetham.io>
-- Stability  : experimental
--
-- Types for the properties described in Section 6 of RFC 6350.
module VCard.Types.Property
  ( -- * Generalized property type
    Property (..),

    -- * Section 6.1 General Properties

    -- ** Section 6.1.1 BEGIN
    Begin,

    -- ** Section 6.1.2 END
    End,

    -- ** Section 6.1.3 SOURCE
    Source,

    -- ** Section 6.1.4 KIND
    Kind,
    KindValue (..),
    KindSymbol,

    -- ** Section 6.1.5 XML
    XML,

    -- * Section 6.2 Identification properties

    -- ** Section 6.2.1 FN
    FN,

    -- ** Section 6.2.2 N
    N,

    -- ** Section 6.2.5 BDAY
    BDay,
    BDayStyle (..),
    BDayParams,
    BDayValue,

    -- * To delete
    Version (..),
  )
where

import GHC.TypeLits (SSymbol, Symbol)
import VCard.Internal.Symbol (ToUpper)
import VCard.Types.Param
import VCard.Types.Property.ParamList (ParamPermutation)
import VCard.Types.Property.Version (Version (..))
import VCard.Types.Value.Text (Text)
import VCard.Types.Value.URI (URI)
import VCard.Types.Value.Time (DateAndOrTime)
import VCard.Types.XName (XNameSymbol)

class KindSymbol (s :: Symbol)

instance KindSymbol "individual"

instance KindSymbol "org"

instance KindSymbol "group"

instance KindSymbol "location"

instance (XNameSymbol x) => KindSymbol s

data Property name_normalized params value where
  Property ::
    (ToUpper name ~ name_normalized) =>
    { propName :: SSymbol name,
      propParams :: ParamPermutation params,
      propValue :: value
    } ->
    Property name_normalized params value

type Begin = Property "BEGIN" '[] (SSymbol "VCARD")

type End = Property "END" '[] (SSymbol "VCARD")

type Source s = Property "SOURCE" '[Value "URI", PID, Pref, AltID s] URI

type Kind s = Property "KIND" '[Value "TEXT"] (KindValue s)

data KindValue s where
  KindValue :: (KindSymbol s) => SSymbol s -> KindValue s

type XML altid = Property "XML" '[Value "text", AltID altid]

type FN altid =
  Property "FN" '[Value "text", Type, Language, AltID altid, PID, Pref] Text

type N altid =
  Property "N" '[Value "text", SortAs, Language, AltID altid] Text

type BDay (bday_style :: BDayStyle) =
  Property "BDAY" (BDayParams bday_style) (BDayValue bday_style)

data BDayStyle = BDayDate | BDayText

type family BDayParams (bday_style :: BDayStyle) where
  BDayParams BDayDate = '[Value "date-and-or-time"]
  BDayParams BDayText = '[Value "text", Language]

type family BDayValue (bday_style :: BDayStyle) where
  BDayValue BDayDate = DateAndOrTime
  BDayValue BDayText = Text
