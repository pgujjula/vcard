-- SPDX-FileCopyrightText: Copyright Preetham Gujjula
-- SPDX-License-Identifier: BSD-3-Clause
{-# OPTIONS_GHC -Wno-orphans #-}

module VCard.Util.Symbol.Private.Parse () where

import Data.Proxy (Proxy (..))
import Data.Text qualified as Text
import GHC.TypeLits (KnownSymbol, symbolVal)
import Text.Megaparsec.Char (string)
import Unsafe.Coerce
import VCard.Parse (HasParser, Parser, parser)
import VCard.Util.Symbol.Private.Compat (SSymbol, withSomeSSymbol)

instance (KnownSymbol s) => HasParser (SSymbol s) where
  parser :: Parser (SSymbol s)
  parser = do
    t <- string (Text.pack (symbolVal (Proxy @s)))
    withSomeSSymbol (Text.unpack t) $ \ss ->
      pure (unsafeCoerce ss)
