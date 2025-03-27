-- SPDX-FileCopyrightText: Copyright Preetham Gujjula
-- SPDX-License-Identifier: BSD-3-Clause
{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneKindSignatures #-}

module VCard.Types.Param.ParamValue
  ( -- * Types
    ParamValue,
    ParamValueSymbol,
    testParamValueSymbol,
    SParamValue (..),
    SomeParamValue (..),

    -- * Construction
    unParamValue,
    paramValueVal,
    someParamValueVal,

    -- * Unquoting
    unquoteParamValue,
    UnquoteParamValueSymbol,
    sUnquoteSParamValue,
  )
where

import Data.Bool.Singletons (SBool (SFalse, STrue))
import Data.Constraint (Dict (..))
import Data.Maybe (isJust)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Type.Bool (If)
import GHC.TypeLits (KnownSymbol, Symbol, symbolVal)
import Text.Megaparsec (choice, takeWhileP)
import Text.Megaparsec.Char (char)
import VCard.Char (dQuote, isQSafeChar, isSafeChar)
import VCard.Parse (HasParser, Parser, parser)
import VCard.Serialize (HasSerializer, Serializer, serializer)
#if MIN_VERSION_GLASGOW_HASKELL(9,4,0,0)
import VCard.Util.Symbol
  ( SSymbol,
    testSSymbolEquality,
    withKnownSymbol,
    withSomeSSymbol,
  )
#else
import VCard.Util.Symbol
  ( SSymbol,
    fromSSymbol,
    testSSymbolEquality,
    withKnownSymbol,
    withSomeSSymbol,
  )
#endif
import VCard.Types.Param.ParamValue.Internal
  ( IsParamValueSymbol,
    UnsafeUnquoteParamValueSymbol,
    sIsParamValueSymbol,
    sUnsafeUnquoteParamValueSymbol,
  )
import VCard.Util (Assert, NoInstance)

-- | A general data type for the value of a parameter. Corresponds to
--   @param-value@ in the ABNF in Section 3.3 of RFC 6350.
newtype ParamValue = ParamValue
  { -- | Unwrap a 'ParamValue'.
    unParamValue :: Text
  }
  deriving (Eq, Show)

-- | Ideally, we would like to promote 'ParamValue' to the kind level. This is
--   not really possible since we don't yet have the tools to work with the
--   underlying 'Text' on the kind level.
--
--   Instead, we create a constraint family 'ParamValueSymbol' that is satisfied
--   by any 'Symbol's that match the pattern of a @param-value@. For example,
--
--     * @'ParamValueSymbol' \"Foo\"@ is satisfied since @Foo@ matches the
--       @*SAFE-CHAR@ pattern.
--
--     * @'ParamValueSymbol' "\\"Foo;\\""@ is satisfied since @"Foo;"@ matches
--       the @DQUOTE *QSAFE-CHAR DQUOTE@ pattern
--
--     * @'ParamValueSymbol' "Foo;"@ is not satisfied, since @Foo;@ contains an
--       unquoted @;@.
type ParamValueSymbol (s :: Symbol) =
  Assert (IsParamValueSymbol s) (NoInstance "ParamValueSymbol" s)

-- | Test a 'Symbol' and potentially get a 'ParamValueSymbol' constraint.
testParamValueSymbol :: SSymbol s -> Maybe (Dict (ParamValueSymbol s))
testParamValueSymbol ss =
  case sIsParamValueSymbol ss of
    STrue -> Just Dict
    SFalse -> Nothing

-- | Singleton type for `ParamValue`.
data SParamValue (s :: Symbol) where
  SParamValue :: (ParamValueSymbol s) => SSymbol s -> SParamValue s

instance Eq (SParamValue s) where
  (==) _ _ = True

deriving instance Show (SParamValue s)

-- | Existential type for 'SParamValue'.
data SomeParamValue where
  SomeParamValue :: SParamValue s -> SomeParamValue

deriving instance Show SomeParamValue

instance Eq SomeParamValue where
  (==) (SomeParamValue (SParamValue ss1)) (SomeParamValue (SParamValue ss2)) =
    isJust (testSSymbolEquality ss1 ss2)

-- | Convert an @'SParamValue' s@ to a 'ParamValue'.
paramValueVal :: SParamValue s -> ParamValue
paramValueVal (SParamValue ss) =
  ParamValue (Text.pack (withKnownSymbol ss (symbolVal ss)))

-- | Convert a 'ParamValue' to an unknown type-level 'SParamValue'.
someParamValueVal :: ParamValue -> SomeParamValue
someParamValueVal (ParamValue x) =
  withSomeSSymbol (Text.unpack x) $ \ss ->
    case testParamValueSymbol ss of
      Nothing ->
        error $
          "someParamValueVal: invalid ParamValue "
            <> "(the \"impossible\" happened)"
      Just Dict -> SomeParamValue (SParamValue ss)

--
-- Parsers
--

instance HasParser ParamValue where
  parser :: Parser ParamValue
  parser =
    ParamValue <$> choice [qsafeP, safeP]
    where
      qsafeP = do
        x <- Text.singleton <$> char dQuote
        y <- takeWhileP (Just "QSAFE") isQSafeChar
        z <- Text.singleton <$> char dQuote
        pure (x <> y <> z)
      safeP = takeWhileP (Just "SAFE") isSafeChar

instance (KnownSymbol s) => HasParser (SParamValue s) where
  parser :: Parser (SParamValue s)
  parser = do
    ss <- parser @(SSymbol s)
    case testParamValueSymbol ss of
      Nothing -> fail ("no SParamValueSymbol instance for " <> show ss)
      Just Dict -> pure (SParamValue ss)

instance HasParser SomeParamValue where
  parser :: Parser SomeParamValue
  parser = someParamValueVal <$> (parser @ParamValue)

--
-- Serializers
--

instance HasSerializer ParamValue where
  serializer :: Serializer ParamValue
  serializer = unParamValue

instance HasSerializer (SParamValue s) where
  serializer :: Serializer (SParamValue s)
  serializer (SParamValue ss) = serializer ss

instance HasSerializer SomeParamValue where
  serializer :: Serializer SomeParamValue
  serializer (SomeParamValue spv) = serializer spv

--
-- Unquoting
--

-- | Remove quotes from a 'ParamValue', if it has them.
--
-- >>> quotedFoo = paramValueVal (SParamValue (symbolSing @"\"foo\""))
-- >>> unquotedFoo = paramValueVal (SParamValue (symbolSing @"foo"))
-- >>> Text.putStrLn (unParamValue quotedFoo)
-- "foo"
-- >>> Text.putStrLn (unquoteParamValue quotedFoo)
-- foo
-- >>> Text.putStrLn (unParamValue unquotedFoo)
-- foo
-- >>> Text.putStrLn (unquoteParamValue unquotedFoo)
-- foo
unquoteParamValue :: ParamValue -> Text
unquoteParamValue (ParamValue t) =
  if Text.take 1 t == "\""
    then Text.drop 1 (Text.take (Text.length t - 1) t)
    else t

-- | Remove quotes from a 'Symbol' with a 'ParamValueSymbol' instance.
--
--   Like 'unquoteParamValue' on the type level.
type UnquoteParamValueSymbol :: Symbol -> Symbol
type UnquoteParamValueSymbol s =
  If
    (IsParamValueSymbol s)
    (UnsafeUnquoteParamValueSymbol s)
    (NoInstance "ParamValueSymbol" s)

-- | Singleton of 'UnquoteParamValueSymbol'.
sUnquoteSParamValue :: SParamValue s -> SSymbol (UnquoteParamValueSymbol s)
sUnquoteSParamValue (SParamValue ss) =
  case sIsParamValueSymbol ss of
    STrue -> sUnsafeUnquoteParamValueSymbol ss
#if !MIN_VERSION_GLASGOW_HASKELL(9,4,0,0)
    -- GHC >= 9.4 is able to deduce that this branch is unreachable, but for GHC
    -- 9.2 we need this error handling
    SFalse ->
      error $ "No instance for (ParamValueSymbol "  <> fromSSymbol ss <> ")"
#endif
