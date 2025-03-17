-- SPDX-FileCopyrightText: Copyright Preetham Gujjula
-- SPDX-License-Identifier: BSD-3-Clause

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
  )
where

import Data.Bool.Singletons (SBool (SFalse, STrue))
import Data.Constraint (Dict (..))
import Data.Text (Text)
import Data.Text qualified as Text
import GHC.TypeLits (KnownSymbol, Symbol, symbolVal)
import Text.Megaparsec (choice, takeWhileP)
import Text.Megaparsec.Char (char)
import VCard.Char (dQuote, isQSafeChar, isSafeChar)
import VCard.Parse (HasParser, Parser, parser)
import VCard.Serialize (HasSerializer, Serializer, serializer)
import VCard.Symbol.Private (SSymbol, withKnownSymbol, withSomeSSymbol)
import VCard.Types.Param.ParamValue.Internal
  ( IsParamValueSymbol,
    sIsParamValueSymbol,
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

-- | Existential type for 'SParamValue'.
data SomeParamValue where
  SomeParamValue :: SParamValue s -> SomeParamValue

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
