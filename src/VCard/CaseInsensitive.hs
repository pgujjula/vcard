-- SPDX-FileCopyrightText: Copyright Preetham Gujjula
-- SPDX-License-Identifier: BSD-3-Clause

module VCard.CaseInsensitive
  ( CaseInsensitiveLower (..),
    CaseInsensitiveUpper (..),
  )
where

import Data.Maybe (isJust)
import Data.Proxy (Proxy (..))
import Data.Text qualified as Text
import Data.Type.Equality ((:~:) (Refl))
import GHC.TypeLits
  ( KnownSymbol,
    SomeSymbol (..),
    Symbol,
    someSymbolVal,
    symbolVal,
  )
import Text.Megaparsec.Char qualified as Megaparsec
import VCard.Parse (HasParser (..), Parser)
import VCard.Serialize (HasSerializer (..), Serializer)
import VCard.Symbol.Private
  ( SSymbol,
    ToLower,
    ToUpper,
    fromSSymbol,
    sToLower,
    sToUpper,
    symbolSing,
    testSSymbolEquality,
  )

-- | Represents a 'Symbol' that, when lowercased, is @s@. For example,
--   @'CaseInsensitiveLower' ('VCard.Symbol.Private.symbolSing' \@\"Foo\")@ has
--   type @'CaseInsensitiveLower' \"foo\"@.
data CaseInsensitiveLower (s :: Symbol) where
  CaseInsensitiveLower ::
    (ToLower t ~ s) =>
    {unCaseInsensitiveLower :: SSymbol t} ->
    CaseInsensitiveLower s

deriving instance Show (CaseInsensitiveLower s)

instance Eq (CaseInsensitiveLower s) where
  (CaseInsensitiveLower st1) == (CaseInsensitiveLower st2) =
    isJust (testSSymbolEquality st1 st2)

instance forall s. (KnownSymbol s) => HasParser (CaseInsensitiveLower s) where
  parser :: Parser (CaseInsensitiveLower s)
  parser = do
    let s = symbolVal (Proxy :: Proxy s)
    let ss = symbolSing @s
    t <- Megaparsec.string' (Text.pack s)
    case someSymbolVal (Text.unpack t) of
      SomeSymbol (Proxy :: Proxy t) ->
        let st = symbolSing @t
         in case testSSymbolEquality (sToLower st) ss of
              Nothing -> fail "CaseInsensitiveLower"
              Just Refl -> pure (CaseInsensitiveLower st)

instance HasSerializer (CaseInsensitiveLower s) where
  serializer :: Serializer (CaseInsensitiveLower s)
  serializer (CaseInsensitiveLower st) = Text.pack (fromSSymbol st)

-- | Represents a 'Symbol' that, when uppercased, is @s@. For example,
--   @'CaseInsensitiveUpper' ('VCard.Symbol.Private.symbolSing' \@\"Foo\")@ has
--   type @'CaseInsensitiveUpper' \"FOO\"@.
data CaseInsensitiveUpper (s :: Symbol) where
  CaseInsensitiveUpper ::
    (ToUpper t ~ s) =>
    {unCaseInsensitiveUpper :: SSymbol t} ->
    CaseInsensitiveUpper s

deriving instance Show (CaseInsensitiveUpper s)

instance Eq (CaseInsensitiveUpper s) where
  (CaseInsensitiveUpper st1) == (CaseInsensitiveUpper st2) =
    isJust (testSSymbolEquality st1 st2)

instance forall s. (KnownSymbol s) => HasParser (CaseInsensitiveUpper s) where
  parser :: Parser (CaseInsensitiveUpper s)
  parser = do
    let s = symbolVal (Proxy :: Proxy s)
    let ss = symbolSing @s
    t <- Megaparsec.string' (Text.pack s)
    case someSymbolVal (Text.unpack t) of
      SomeSymbol (Proxy :: Proxy t) ->
        let st = symbolSing @t
         in case testSSymbolEquality (sToUpper st) ss of
              Nothing -> fail "CaseInsensitiveUpper"
              Just Refl -> pure (CaseInsensitiveUpper st)

instance HasSerializer (CaseInsensitiveUpper s) where
  serializer :: Serializer (CaseInsensitiveUpper s)
  serializer (CaseInsensitiveUpper st) = Text.pack (fromSSymbol st)
