-- SPDX-FileCopyrightText: Copyright Preetham Gujjula
-- SPDX-License-Identifier: BSD-3-Clause
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-}

module VCard.Types.Param.Calscale.CalscaleValueSymbol
  ( CalscaleValueSymbol,
    testCalscaleValueSymbol,
  )
where

import Data.Constraint (Dict (..))
import Data.Proxy (Proxy (..))
import Data.Type.Equality ((:~:) (Refl))
import GHC.TypeLits (Symbol)
import VCard.Internal.Closed (Closed)
import VCard.Symbol.Private (SSymbol, symbolSing, testSSymbolEquality)
import VCard.XName (XNameLowerSymbol, testXNameLowerSymbol)

-- | 'CalscaleValueSymbol' is a closed class, attempting to define a new
--   instance will result in a compilation error.
class CalscaleValueSymbol (s :: Symbol) where
  default _privateCalscaleValueSymbol :: (Closed CalscaleValueSymbol) => Proxy s
  _privateCalscaleValueSymbol :: Proxy s
  _privateCalscaleValueSymbol = Proxy

instance {-# INCOHERENT #-} CalscaleValueSymbol "gregorian" where
  _privateCalscaleValueSymbol = Proxy

instance
  {-# OVERLAPPABLE #-}
  (XNameLowerSymbol s) =>
  CalscaleValueSymbol s
  where
  _privateCalscaleValueSymbol = Proxy

testCalscaleValueSymbol ::
  forall s. SSymbol s -> Maybe (Dict (CalscaleValueSymbol s))
testCalscaleValueSymbol ss =
  testLiteral (symbolSing @"gregorian") $
    case testXNameLowerSymbol ss of
      Nothing -> Nothing
      Just Dict -> Just Dict
  where
    testLiteral ::
      forall t.
      (CalscaleValueSymbol t) =>
      SSymbol t ->
      Maybe (Dict (CalscaleValueSymbol s)) ->
      Maybe (Dict (CalscaleValueSymbol s))
    testLiteral st x =
      case testSSymbolEquality ss st of
        Just Refl -> Just (Dict @(CalscaleValueSymbol t))
        Nothing -> x
