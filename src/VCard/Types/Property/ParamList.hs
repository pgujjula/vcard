-- SPDX-FileCopyrightText: Copyright Preetham Gujjula
-- SPDX-License-Identifier: BSD-3-Clause
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-}

module VCard.Types.Property.ParamList
  ( ParamList (..),
    ParamPermutation (..),
    Param (..),
    get,
    get'
  )
where

import Data.Kind (Type)
import Data.Type.Bool (If, type (||))
import Data.Type.Equality (testEquality, type (==))
import Data.Typeable (Typeable, (:~:) (Refl))
import GHC.TypeLits (KnownSymbol, SSymbol, Symbol)
import Type.Reflection (TypeRep, pattern TypeRep)
import VCard.Internal.Symbol (ToUpper)

type Elem x xs = IsElem x xs ~ True

data ParamList xs where
  Nil :: ParamList '[]
  (:.) :: Param name value -> ParamList xs -> ParamList (Param name value ': xs)

data ParamPermutation (xs :: [Type]) where
  ParamPermutation :: (PermutationOf xs ys) => ParamList ys -> ParamPermutation xs

data Param (name_normalized :: Symbol) (value :: Type) where
  Param ::
    (KnownSymbol name_normalized, ToUpper name ~ name_normalized, Typeable value) =>
    { valueParamName :: SSymbol name,
      valueParamValue :: value
    } ->
    Param name_normalized value

type family IsElem (x :: Type) (xs :: [Type]) :: Bool where
  IsElem x '[] = False
  IsElem x (y ': ys) = x == y || IsElem x ys

get :: (Typeable x, Elem x xs) => ParamList xs -> x
get = getUnsafe

getUnsafe :: forall x xs. (Typeable x) => ParamList xs -> x
getUnsafe = \case
  Nil -> error "impossible"
  (n :: Param name value) :. (ns :: ParamList ys) ->
    let t3 :: TypeRep (Param name value)
        t3 = case n of Param {} -> TypeRep

     in case testEquality t3 (TypeRep :: TypeRep x) of
          Nothing -> getUnsafe ns
          Just Refl -> n

get' :: (Typeable x, Elem x xs) => ParamPermutation xs -> x
get' (ParamPermutation paramList) = getUnsafe paramList

deriving instance Eq (ParamList '[])

deriving instance (Eq x, Eq (ParamList xs)) => Eq (ParamList (x ': xs))

deriving instance Show (ParamList '[])

deriving instance (Show x, Show (ParamList xs)) => Show (ParamList (x ': xs))

type PermutationOf xs ys = IsPermutationOf xs ys ~ True

type family IsPermutationOf (xs :: [Type]) (ys :: [Type]) :: Bool where
  IsPermutationOf '[] '[] = True
  IsPermutationOf '[] (y ': ys) = False
  IsPermutationOf (x ': xs) ys =
    IsPermutationOfMaybe xs (Extract x ys)

type family Extract (x :: Type) (ys :: [Type]) :: Maybe [Type] where
  Extract x '[] = Nothing
  Extract x (y ': ys) = If (x == y) (Just ys) (MaybeCons y (Extract x ys))

type family MaybeCons (x :: Type) (mxs :: Maybe [Type]) :: Maybe [Type] where
  MaybeCons x Nothing = Nothing
  MaybeCons x (Just xs) = Just (x ': xs)

type family IsPermutationOfMaybe (xs :: [Type]) (mys :: Maybe [Type]) :: Bool where
  IsPermutationOfMaybe xs Nothing = False
  IsPermutationOfMaybe xs (Just ys) = IsPermutationOf xs ys
