-- SPDX-FileCopyrightText: Copyright Preetham Gujjula
-- SPDX-License-Identifier: BSD-3-Clause
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

-- |
-- Module     : VCard.Types.Property.ParamMap1
-- Copyright  : Copyright Preetham Gujjula
-- License    : BSD-3-Clause
-- Maintainer : Preetham Gujjula <libraries@mail.preetham.io>
-- Stability  : experimental
module VCard.Types.Property.ParamMap1
  ( -- ParamMap,
    --    lookupParam,
    --    appendParam,
    --    prependParam,
    --    deleteParam,
    --    lookupParamIndex,
    --    assocs,
    --    keys,
    --    SomeParam,
    --    SomeParamKey,
    ParamTag,
    SParamTag (..),
    --    LookupParamType,
    --    ValidParamName,
  )
where

import Data.Bool.Singletons (If, SBool (..), sIf)
import Data.Dependent.Map (DMap)
import Data.Eq.Singletons ((%==))
import Data.Functor.Identity (Identity (..))
import Data.Kind (Constraint, Type)
import Data.List.Singletons (SList (..))
import Data.Maybe.Singletons (FromMaybe, SMaybe (SJust, SNothing))
import Data.Proxy (Proxy (..))
import Data.Singletons (Sing)
import GHC.TypeLits (ErrorMessage (..), SSymbol, Symbol, TypeError)
import Prelude.Singletons (type (==))
import VCard.Types.Param (LanguageParam)
import VCard.Types.Textual
  ( CaseInsensitiveUpper (..),
    IsXNameUpperSymbol,
    sIsXNameUpperSymbol,
  )
import VCard.Util.Symbol (sToUpper, symbolSing)

-- -- | A generic type for parameters. The different vCard parameters are type
-- --   synonyms of this type.
-- data GenericProperty (name :: Symbol) (params :: [ParamTag]) (value :: Type)
--   = GenericProperty
--   { genericPropertyName :: CaseInsensitiveUpper name,
--     genericPropertyParams :: ParamMap params,
--     genericPropertyValue :: value
--   }

data ParamMap (tags :: [ParamTag])
  = ParamMap
  { dmap :: DMap ParamKey Identity,
    stags :: SList tags
  }

data ParamKey a where
  LanguageParamKey :: ParamKey LanguageParam

--  ValueParamKey :: SSymbol s -> ParamKey (ValueParam s)
--  PrefParamKey :: ParamKey PrefParam
--  AltIDParamKey :: ParamKey AltIDParam
--  PIDParamKey :: ParamKey PIDParam
--  MediatypeParamKey :: ParamKey MediatypeParam
--  SortAsParamKey :: ParamKey SortAsParam
--  TypeParamKey :: ParamKey (TypeParam symbol_class)
--  CalscaleParamKey :: SSymbol s -> ParamKey (CalscaleParam s)
--  GeoParamKey :: ParamKey GeoParam
--  TzParamKey :: ParamKey TzParam
--  XParamKey :: SXName s -> ParamKey (GenericParam s (NonEmpty ParamValue))

data ParamTag where
  LanguageParamTag :: ParamTag
  ValueParamTag :: Symbol -> ParamTag
  PrefParamTag :: ParamTag
  AltIDParamTag :: ParamTag
  PIDParamTag :: ParamTag
  TypeParamTag :: (Symbol -> Constraint) -> ParamTag
  MediatypeParamTag :: ParamTag
  SortAsParamTag :: ParamTag
  CalscaleParamTag :: ParamTag
  GeoParamTag :: ParamTag
  TzParamTag :: ParamTag
  AnyParamTag :: ParamTag

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
  SCalscaleParamTag :: SSymbol s -> SParamTag CalscaleParamTag
  SGeoParamTag :: SParamTag GeoParamTag
  STzParamTag :: SParamTag TzParamTag
  SAnyParamTag :: SParamTag AnyParamTag

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
  ValidParamTag s CalscaleParamTag = s == "CALSCALE"
  ValidParamTag s GeoParamTag = s == "GEO"
  ValidParamTag s TzParamTag = s == "TZ"
  ValidParamTag s AnyParamTag = IsXNameUpperSymbol s

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
    SAnyParamTag -> sIsXNameUpperSymbol ss

type LookupParamType :: Symbol -> [ParamTag] -> Type
type LookupParamType s tags =
  FromMaybe
    (TypeError (Text "Symbol " :<>: Text s :<>: Text " was not found in " :<>: ShowType tags))
    (LookupParamTypeMaybe s tags)

type ValidParamName :: Symbol -> [ParamTag] -> Constraint
type ValidParamName s tags = LookupParamTypeMaybe s tags ~ Nothing

type LookupParamTypeMaybe :: Symbol -> [ParamTag] -> Maybe Type
type family LookupParamTypeMaybe s tags where
  LookupParamTypeMaybe "LANGUAGE" '[] = Nothing
  LookupParamTypeMaybe "LANGUAGE" (LanguageParamTag ': _) = Just LanguageParam
  LookupParamTypeMaybe "LANGUAGE" (_ ': xs) = LookupParamTypeMaybe "LANGUAGE" xs

lookupParam :: forall name tags value. (LookupParamType name tags ~ value) => CaseInsensitiveUpper name -> ParamMap tags -> Maybe value
lookupParam (CaseInsensitiveUpper name') pmap =
  let stags' :: SList tags
      stags' = stags pmap

      sname :: SSymbol name
      sname = sToUpper name'
   in undefined

-- appendParam :: (LookupParamType name tags ~ value) => CaseInsensitiveUpper name -> value -> ParamMap tags -> ParamMap tags
-- appendParam = undefined
--
-- prependParam :: (LookupParamType name tags ~ value) => CaseInsensitiveUpper name -> value -> ParamMap tags -> ParamMap tags
-- prependParam = undefined
--

deleteParam :: (ValidParamName name tags) => CaseInsensitiveUpper name -> ParamMap tags -> ParamMap tags
deleteParam = undefined

-- lookupParamIndex :: (ValidParamName name tags) => CaseInsensitiveUpper name -> ParamMap tags -> Maybe Word
-- lookupParamIndex = undefined

-- data SomeParamKey tags where
--  SomeParamKey :: (ValidParamName name tags) => CaseInsensitiveUpper name -> SomeParamKey tags
--
-- data SomeParam tags where
--  SomeParam :: (LookupParamType name tags ~ value) => CaseInsensitiveUpper name -> value -> SomeParam tags
--
-- keys :: ParamMap tags -> [SomeParamKey tags]
-- keys = undefined
--
-- assocs :: ParamMap tags -> [SomeParam tags]
-- assocs = undefined
