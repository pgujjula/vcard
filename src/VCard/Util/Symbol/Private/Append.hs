-- SPDX-FileCopyrightText: Copyright Preetham Gujjula
-- SPDX-License-Identifier: BSD-3-Clause
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module VCard.Util.Symbol.Private.Append
  ( sAppendSymbol,
  )
where

import GHC.TypeLits (AppendSymbol)
import Unsafe.Coerce (unsafeCoerce)
import VCard.Util.Symbol.Private.Compat (SSymbol, fromSSymbol, withSomeSSymbol)

sAppendSymbol :: SSymbol a -> SSymbol b -> SSymbol (AppendSymbol a b)
sAppendSymbol sa sb =
  withSomeSSymbol (fromSSymbol sa ++ fromSSymbol sb) unsafeCoerce
