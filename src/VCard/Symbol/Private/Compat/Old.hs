-- SPDX-FileCopyrightText: Copyright Preetham Gujjula
-- SPDX-License-Identifier: BSD-3-Clause
module VCard.Symbol.Private.Compat.Old (charSing, symbolSing) where

import GHC.TypeLits (KnownChar, KnownSymbol)
import GHC.TypeLits.Singletons (SChar (..), SSymbol (SSym))

charSing :: (KnownChar c) => SChar c
charSing = SChar

symbolSing :: (KnownSymbol s) => SSymbol s
symbolSing = SSym
