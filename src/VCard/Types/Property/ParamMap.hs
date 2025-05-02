-- SPDX-FileCopyrightText: Copyright Preetham Gujjula
-- SPDX-License-Identifier: BSD-3-Clause
{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

-- |
-- Module     : VCard.Types.Property.ParamMap
-- Copyright  : Copyright Preetham Gujjula
-- License    : BSD-3-Clause
-- Maintainer : Preetham Gujjula <libraries@mail.preetham.io>
-- Stability  : experimental
module VCard.Types.Property.ParamMap
  ( ParamMap,
  )
where

import Data.Bool.Singletons (If, SBool (..), sIf)
import Data.Dependent.Map (DMap)
import Data.Dependent.Map qualified as DMap
import Data.Dependent.Sum (DSum ((:=>)), (==>))
import Data.Dependent.Sum qualified as DSum
import Data.Eq.Singletons ((%==))
import Data.Foldable.Singletons (Asum)
import Data.Functor.Identity (Identity (..))
import Data.Kind (Constraint, Type)
import Data.List.NonEmpty (NonEmpty)
import Data.List.Singletons (SList (..))
import Data.Maybe.Singletons (FromMaybe, IsJust, SMaybe (SJust, SNothing))
import Data.Proxy (KProxy (..), Proxy (..))
import Data.Record.Anon ()
import Data.Record.Anon.Advanced ()
import Data.Singletons (Sing)
import Data.Singletons.Base.TypeError qualified as S
import Data.Type.Equality ((:~:) (Refl))
import Data.Void (Void)
import GHC.TypeError (Assert, ErrorMessage (ShowType, Text, (:<>:)), TypeError)
import GHC.TypeLits (SSymbol (..), Symbol)
import Prelude.Singletons (type (==))
import VCard.Types.Param
  ( AltIDParam,
    CalscaleParam,
    CalscaleValue,
    GenericParam (..),
    GeoParam,
    GeoValue,
    LanguageParam,
    Mediatype,
    MediatypeParam,
    PIDParam,
    PIDValue,
    ParamValue,
    PrefParam,
    PrefValue,
    SortAsParam,
    TypeParam,
    TypeValue,
    TzParam,
    TzValue,
    ValueParam,
    ValueValue (..),
  )
import VCard.Types.Textual
  ( CaseInsensitiveLower (..),
    CaseInsensitiveUpper (..),
    IsXNameUpperSymbol,
    SXName (..),
    XNameUpperSymbol (..),
    sIsXNameUpperSymbol,
  )
import VCard.Types.Value (LanguageTag)
import VCard.Util.Symbol (SSymbol, symbolSing)

data ParamMap tags
  = ParamMap
  { getParamMap :: DMap ParamKey Identity,
    getSTags :: SList tags
  }

data ParamTag where
  LanguageParamTag :: ParamTag
  ValueParamTag :: Symbol -> ParamTag
  PrefParamTag :: ParamTag
  AltIDParamTag :: ParamTag
  PIDParamTag :: ParamTag
  TypeParamTag :: (Symbol -> Constraint) -> ParamTag
  MediatypeParamTag :: ParamTag
  SortAsParamTag :: ParamTag
  CalscaleParamTag :: Symbol -> ParamTag
  GeoParamTag :: ParamTag
  TzParamTag :: ParamTag
  XParamTag :: ParamTag

type instance Sing @ParamTag = SParamTag

data SParamTag (tag :: ParamTag) where
  SLanguageParamTag :: SParamTag LanguageParamTag
  SValueParamTag :: SSymbol s -> SParamTag (ValueParamTag s)
  SPrefParamTag :: SParamTag PrefParamTag
  SAltIDParamTag :: SParamTag AltIDParamTag
  SPIDParamTag :: SParamTag PIDParamTag
  STypeParamTag :: Proxy (f :: Symbol -> Constraint) -> SParamTag (TypeParamTag f)
  SMediatypeParamTag :: SParamTag MediatypeParamTag
  SSortAsParamTag :: SParamTag SortAsParamTag
  SCalscaleParamTag :: SSymbol s -> SParamTag (CalscaleParamTag s)
  SGeoParamTag :: SParamTag GeoParamTag
  STzParamTag :: SParamTag TzParamTag
  SXParamTag :: SParamTag XParamTag

data SomeParamKey where
  SomeParamKey :: ParamKey a -> SomeParamKey

data SomeGenericParam where
  SomeGenericParam :: Symbol -> Type -> SomeGenericParam

data ParamKey a where
  LanguageParamKey :: ParamKey LanguageParam
  ValueParamKey :: SSymbol s -> ParamKey (ValueParam s)
  PrefParamKey :: ParamKey PrefParam
  AltIDParamKey :: ParamKey AltIDParam
  PIDParamKey :: ParamKey PIDParam
  TypeParamKey :: Proxy (p :: Symbol -> Constraint) -> ParamKey (TypeParam p)
  MediatypeParamKey :: ParamKey MediatypeParam
  SortAsParamKey :: ParamKey SortAsParam
  CalscaleParamKey :: SSymbol s -> ParamKey (CalscaleParam s)
  GeoParamKey :: ParamKey GeoParam
  TzParamKey :: ParamKey TzParam
  XParamKey :: SXName s -> ParamKey (GenericParam s (NonEmpty ParamValue))

getParamValue ::
  forall value name tags.
  (value ~ LookupParamValue3 name tags) =>
  SSymbol name ->
  ParamMap tags ->
  Maybe value
getParamValue sname pmap =
  let stags = getSTags pmap
      stagMaybe = sMatchParamTagMaybe sname stags
   in case stagMaybe of
        SJust SXParamTag -> undefined
        SNothing ->
          let p1 :: MatchParamTagMaybe name tags :~: Nothing
              p1 = Refl
              p2 :: MatchParamTag name tags :~: Unmaybe3 Nothing
              p2 = Refl
              p3 :: MatchParamTag name tags :~: S.TypeError (S.Text "Unmaybe3: Nothing")
              p3 = Refl
              p4 :: LookupParamValue3 name tags :~: GetValue1 name (S.TypeError (S.Text "Unmaybe3: Nothing"))
              p4 = Refl
              p5 ::
                value
                  :~: GetValue1 name (S.TypeError (S.Text "Unmaybe3: Nothing"))
              p5 = Refl
           in Nothing

getParamValue1 ::
  (value ~ LookupParamValue3 name tags) =>
  SSymbol name ->
  ParamMap tags ->
  Maybe value
getParamValue1 sname pmap = undefined

getParam ::
  (value ~ LookupParamValue name tags) =>
  SSymbol name ->
  ParamMap tags ->
  Maybe (GenericParam name value)
getParam = undefined

setParam ::
  (value ~ LookupParamValue name tags) =>
  GenericParam name value ->
  ParamMap tags ->
  ParamMap tags
setParam = undefined

deleteParam ::
  (MatchParamName name tags) =>
  SSymbol name ->
  ParamMap tags ->
  ParamMap tags
deleteParam = undefined

type MatchParamName :: Symbol -> [ParamTag] -> Constraint
type MatchParamName s tags =
  Assert
    (IsJust (LookupParamValueMaybe s tags))
    ( TypeError
        ( Text "MatchParamName: No matching tag for "
            :<>: ShowType s
            :<>: Text " in "
            :<>: ShowType tags
        )
    )

type LookupParamValue :: Symbol -> [ParamTag] -> Type
type LookupParamValue s tags =
  FromMaybe
    ( TypeError
        ( Text "LookupParamValue: No matching tag for "
            :<>: ShowType s
            :<>: Text " in "
            :<>: ShowType tags
        )
    )
    (LookupParamValueMaybe s tags)

type LookupParamValue1 :: Symbol -> [ParamTag] -> Type
type LookupParamValue1 s tags = GetValue (LookupParam s tags)

type GetValue :: SomeGenericParam -> Type
type family GetValue p where
  GetValue ('SomeGenericParam name value) = value

type LookupParam :: Symbol -> [ParamTag] -> SomeGenericParam
type LookupParam s tags =
  FromMaybe
    ( TypeError
        ( Text "LookupParam: No matching tag for "
            :<>: ShowType s
            :<>: Text " in "
            :<>: ShowType tags
        )
    )
    (LookupParamKey1Maybe s tags)

type LookupParamValueMaybe :: Symbol -> [ParamTag] -> Maybe Type
type family LookupParamValueMaybe s tags where
  LookupParamValueMaybe s tags = Asum (MapMatchParamValue s tags)

type LookupParamKey1Maybe :: Symbol -> [ParamTag] -> Maybe SomeGenericParam
type family LookupParamKey1Maybe s tags where
  LookupParamKey1Maybe s tags = Asum (MapMatchParamKey1 s tags)

type MapMatchParamValue :: Symbol -> [ParamTag] -> [Maybe Type]
type family MapMatchParamValue s tags where
  MapMatchParamValue s '[] = '[]
  MapMatchParamValue s (tag : tags) =
    MatchParamValue s tag : MapMatchParamValue s tags

type LookupParamValue3 :: Symbol -> [ParamTag] -> Type
type LookupParamValue3 s tags = GetValue1 s (MatchParamTag s tags)

type Unmaybe1 :: Maybe Bool -> Bool
type family Unmaybe1 mt where
  Unmaybe1 Nothing = TypeError (Text "Unmaybe1: Oops")
  Unmaybe1 (Just True) = True
  Unmaybe1 (Just False) = False

type Unmaybe :: Maybe Type -> Type
type family Unmaybe mt where
  Unmaybe Nothing = TypeError (Text "LookupParam3: No matching tag")
  Unmaybe (Just t) = t

type LookupParamValue2 :: Symbol -> [ParamTag] -> Maybe Type
type family LookupParamValue2 s tags where
  LookupParamValue2 s tags = AttachValueMaybe s (MatchParamTagMaybe s tags)

type Unmaybe3 :: Maybe ParamTag -> ParamTag
type family Unmaybe3 mpt where
  Unmaybe3 Nothing = S.TypeError (S.Text "Unmaybe3: Nothing")
  Unmaybe3 (Just x) = x

sUnmaybe3 :: SMaybe mpt -> SParamTag (Unmaybe3 mpt)
sUnmaybe3 SNothing = S.sTypeError (S.SText (symbolSing @"Unmaybe3: Nothing"))
sUnmaybe3 (SJust stag) = stag

type AttachValueMaybe :: Symbol -> Maybe ParamTag -> Maybe Type
type family AttachValueMaybe s mtag where
  AttachValueMaybe s Nothing = Nothing
  AttachValueMaybe s (Just tag) = Just (GetValue1 s tag)

type MatchParamTag :: Symbol -> [ParamTag] -> ParamTag
type family MatchParamTag s tags where
  MatchParamTag s tags = Unmaybe3 (MatchParamTagMaybe s tags)

sMatchParamTag ::
  SSymbol s -> SList (tags :: [ParamTag]) -> SParamTag (MatchParamTag s tags)
sMatchParamTag ss stags = sUnmaybe3 (sMatchParamTagMaybe ss stags)

proveUnmaybe ::
  forall x. SMaybe x -> Unmaybe3 x :~: XParamTag -> x :~: Just XParamTag
proveUnmaybe smx Refl =
  case smx of
    SNothing ->
      let p1 :: x :~: Nothing
          p1 = Refl

          p2 :: Unmaybe3 Nothing :~: XParamTag
          p2 = Refl
          p3 :: S.TypeError (S.Text "Unmaybe3: Nothing") :~: XParamTag
          p3 = Refl

          p4 :: Just (S.TypeError (S.Text "Unmaybe3: Nothing")) :~: Just XParamTag
          p4 = Refl
       in case sUnmaybe3 smx of
            SXParamTag -> undefined
    SJust _ -> Refl

type MatchParamTagMaybe :: Symbol -> [ParamTag] -> Maybe ParamTag
type family MatchParamTagMaybe s tags where
  MatchParamTagMaybe s '[] = Nothing
  MatchParamTagMaybe s (tag : tags) =
    If (ValidParamTag s tag) (Just tag) (MatchParamTagMaybe s tags)

sMatchParamTagMaybe ::
  SSymbol s ->
  SList (tags :: [ParamTag]) ->
  SMaybe (MatchParamTagMaybe s tags)
sMatchParamTagMaybe _ SNil = SNothing
sMatchParamTagMaybe ss (SCons stag stags) =
  sIf (sValidParamTag ss stag) (SJust stag) (sMatchParamTagMaybe ss stags)

type MapMatchParamKey1 :: Symbol -> [ParamTag] -> [Maybe SomeGenericParam]
type family MapMatchParamKey1 s tags where
  MapMatchParamKey1 s '[] = '[]
  MapMatchParamKey1 s (tag : tags) =
    MatchParamKey1 s tag : MapMatchParamKey1 s tags

type ValidParamTag :: Symbol -> ParamTag -> Bool
type family ValidParamTag s tag where
  ValidParamTag s LanguageParamTag = s == "LANGUAGE"
  ValidParamTag s (ValueParamTag v) = s == "VALUE"
  ValidParamTag s PrefParamTag = s == "PREF"
  ValidParamTag s AltIDParamTag = s == "ALTID"
  ValidParamTag s PIDParamTag = s == "PID"
  ValidParamTag s (TypeParamTag f) = s == "TYPE"
  ValidParamTag s MediatypeParamTag = s == "MEDIATYPE"
  ValidParamTag s SortAsParamTag = s == "SORT-AS"
  ValidParamTag s (CalscaleParamTag c) = s == "CALSCALE"
  ValidParamTag s GeoParamTag = s == "GEO"
  ValidParamTag s TzParamTag = s == "TZ"
  ValidParamTag s XParamTag = IsXNameUpperSymbol s

sValidParamTag :: SSymbol s -> SParamTag tag -> SBool (ValidParamTag s tag)
sValidParamTag ss stag =
  case stag of
    SLanguageParamTag -> ss %== symbolSing @"LANGUAGE"
    SValueParamTag _ -> ss %== symbolSing @"VALUE"
    SPrefParamTag -> ss %== symbolSing @"PREF"
    SAltIDParamTag -> ss %== symbolSing @"ALTID"
    SPIDParamTag -> ss %== symbolSing @"PID"
    STypeParamTag _ -> ss %== symbolSing @"TYPE"
    SMediatypeParamTag -> ss %== symbolSing @"MEDIATYPE"
    SSortAsParamTag -> ss %== symbolSing @"SORT-AS"
    (SCalscaleParamTag _) -> ss %== symbolSing @"CALSCALE"
    SGeoParamTag -> ss %== symbolSing @"GEO"
    STzParamTag -> ss %== symbolSing @"TZ"
    SXParamTag -> sIsXNameUpperSymbol ss

proveXName :: SSymbol s -> ValidParamTag s XParamTag :~: True -> IsXNameUpperSymbol s :~: True
proveXName _ Refl = Refl

proveXName' ::
  forall s tags.
  SSymbol s ->
  SList tags ->
  MatchParamTagMaybe s tags :~: Just XParamTag ->
  IsXNameUpperSymbol s :~: True
proveXName' ss stags Refl =
  case stags of
    SCons (stag :: SParamTag tag) stags' ->
      case sValidParamTag ss stag of
        STrue -> proveXName ss Refl
        SFalse -> proveXName' ss stags' Refl

proveXName'' ::
  forall s tags.
  SSymbol s ->
  SList tags ->
  MatchParamTag s tags :~: XParamTag ->
  IsXNameUpperSymbol s :~: True
proveXName'' ss stags Refl =
  let p1 :: Unmaybe3 (MatchParamTagMaybe s tags) :~: XParamTag
      p1 = Refl
      smp :: SMaybe (MatchParamTagMaybe s tags)
      smp = sMatchParamTagMaybe ss stags
   in case smp of
        SNothing ->
          let p1 :: MatchParamTagMaybe s tags :~: Nothing
              p1 = Refl
              p2 :: Unmaybe3 Nothing :~: XParamTag
              p2 = Refl
           in undefined
        SJust _ -> proveXName' ss stags Refl

type MatchParamKey1 :: Symbol -> ParamTag -> Maybe SomeGenericParam
type family MatchParamKey1 s tag where
  MatchParamKey1 s LanguageParamTag =
    If (s == "LANGUAGE") (Just ('SomeGenericParam "LANGUAGE" LanguageTag)) Nothing
  MatchParamKey1 s (ValueParamTag v) =
    If (s == "VALUE") (Just ('SomeGenericParam "VALUE" (ValueValue s))) Nothing
  MatchParamKey1 s PrefParamTag =
    If (s == "PREF") (Just ('SomeGenericParam "PREF" PrefValue)) Nothing
  MatchParamKey1 s XParamTag =
    If (IsXNameUpperSymbol s) (Just ('SomeGenericParam s (NonEmpty ParamValue))) Nothing

type GetValue1 :: Symbol -> ParamTag -> Type
type family GetValue1 s tag where
  GetValue1 s LanguageParamTag = LanguageTag
  GetValue1 s (ValueParamTag v) = ValueValue v
  GetValue1 s PrefParamTag = PrefValue
  GetValue1 s AltIDParamTag = ParamValue
  GetValue1 s PIDParamTag = PIDValue
  GetValue1 s (TypeParamTag sclass) = TypeValue sclass
  GetValue1 s MediatypeParamTag = Mediatype
  GetValue1 s (CalscaleParamTag c) = CalscaleValue c
  GetValue1 s SortAsParamTag = NonEmpty ParamValue
  GetValue1 s GeoParamTag = GeoValue
  GetValue1 s TzParamTag = TzValue
  GetValue1 s XParamTag = NonEmpty ParamValue

type MatchParamValue :: Symbol -> ParamTag -> Maybe Type
type family MatchParamValue s tag where
  MatchParamValue s LanguageParamTag =
    If (s == "LANGUAGE") (Just LanguageTag) Nothing
  MatchParamValue s (ValueParamTag v) =
    If (s == "VALUE") (Just (ValueValue v)) Nothing
  MatchParamValue s PrefParamTag =
    If (s == "PREF") (Just PrefValue) Nothing
  MatchParamValue s AltIDParamTag =
    If (s == "ALTID") (Just ParamValue) Nothing
  MatchParamValue s PIDParamTag =
    If (s == "PID") (Just PIDValue) Nothing
  MatchParamValue s (TypeParamTag sclass) =
    If (s == "TYPE") (Just (TypeValue sclass)) Nothing
  MatchParamValue s MediatypeParamTag =
    If (s == "MEDIATYPE") (Just Mediatype) Nothing
  MatchParamValue s (CalscaleParamTag c) =
    If (s == "CALSCALE") (Just (CalscaleValue c)) Nothing
  MatchParamValue s SortAsParamTag =
    If (s == "SORT-AS") (Just (NonEmpty ParamValue)) Nothing
  MatchParamValue s GeoParamTag =
    If (s == "GEO") (Just GeoValue) Nothing
  MatchParamValue s TzParamTag =
    If (s == "TZ") (Just TzValue) Nothing
  MatchParamValue s XParamTag =
    If (IsXNameUpperSymbol s) (Just ParamValue) Nothing
