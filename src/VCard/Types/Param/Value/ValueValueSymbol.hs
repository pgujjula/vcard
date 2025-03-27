-- SPDX-FileCopyrightText: Copyright Preetham Gujjula
-- SPDX-License-Identifier: BSD-3-Clause
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-}

module VCard.Types.Param.Value.ValueValueSymbol
  ( ValueValueSymbol,
    testValueValueSymbol,
  )
where

import Data.Constraint (Dict (..))
import Data.Proxy (Proxy (..))
import Data.Type.Equality ((:~:) (Refl))
import GHC.TypeLits (Symbol)
import VCard.Internal.Closed (Closed)
import VCard.Symbol.Private (SSymbol, symbolSing, testSSymbolEquality)
import VCard.Types.Textual.Private.XName
  ( XNameLowerSymbol,
    testXNameLowerSymbol,
  )

-- | 'ValueValueSymbol' is a closed class, attempting to define a new instance
--   will result in a compiler error.
class ValueValueSymbol (s :: Symbol) where
  default _privateValueValueSymbol :: (Closed ValueValueSymbol) => Proxy s
  _privateValueValueSymbol :: Proxy s
  _privateValueValueSymbol = Proxy

instance {-# INCOHERENT #-} ValueValueSymbol "text" where
  _privateValueValueSymbol = Proxy

instance {-# INCOHERENT #-} ValueValueSymbol "uri" where
  _privateValueValueSymbol = Proxy

instance {-# INCOHERENT #-} ValueValueSymbol "date" where
  _privateValueValueSymbol = Proxy

instance {-# INCOHERENT #-} ValueValueSymbol "time" where
  _privateValueValueSymbol = Proxy

instance {-# INCOHERENT #-} ValueValueSymbol "date-time" where
  _privateValueValueSymbol = Proxy

instance {-# INCOHERENT #-} ValueValueSymbol "date-and-or-time" where
  _privateValueValueSymbol = Proxy

instance {-# INCOHERENT #-} ValueValueSymbol "timestamp" where
  _privateValueValueSymbol = Proxy

instance {-# INCOHERENT #-} ValueValueSymbol "boolean" where
  _privateValueValueSymbol = Proxy

instance {-# INCOHERENT #-} ValueValueSymbol "integer" where
  _privateValueValueSymbol = Proxy

instance {-# INCOHERENT #-} ValueValueSymbol "float" where
  _privateValueValueSymbol = Proxy

instance {-# INCOHERENT #-} ValueValueSymbol "utc-offset" where
  _privateValueValueSymbol = Proxy

instance {-# INCOHERENT #-} ValueValueSymbol "language-tag" where
  _privateValueValueSymbol = Proxy

instance {-# OVERLAPPABLE #-} (XNameLowerSymbol s) => ValueValueSymbol s where
  _privateValueValueSymbol = Proxy

testValueValueSymbol :: forall s. SSymbol s -> Maybe (Dict (ValueValueSymbol s))
testValueValueSymbol ss =
  testLiteral (symbolSing @"text")
    . testLiteral (symbolSing @"uri")
    . testLiteral (symbolSing @"date")
    . testLiteral (symbolSing @"time")
    . testLiteral (symbolSing @"date-time")
    . testLiteral (symbolSing @"date-and-or-time")
    . testLiteral (symbolSing @"timestamp")
    . testLiteral (symbolSing @"boolean")
    . testLiteral (symbolSing @"integer")
    . testLiteral (symbolSing @"float")
    . testLiteral (symbolSing @"utc-offset")
    . testLiteral (symbolSing @"language-tag")
    $ case testXNameLowerSymbol ss of
      Nothing -> Nothing
      Just Dict -> Just Dict
  where
    testLiteral ::
      forall t.
      (ValueValueSymbol t) =>
      SSymbol t ->
      Maybe (Dict (ValueValueSymbol s)) ->
      Maybe (Dict (ValueValueSymbol s))
    testLiteral st x =
      case testSSymbolEquality ss st of
        Just Refl -> Just (Dict @(ValueValueSymbol t))
        Nothing -> x
