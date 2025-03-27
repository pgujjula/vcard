-- SPDX-FileCopyrightText: Copyright Preetham Gujjula
-- SPDX-License-Identifier: BSD-3-Clause
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module VCard.Types.Textual.Private.XName
  ( -- * XName
    XNameSymbol,
    IsXNameSymbol,
    sIsXNameSymbol,
    testXNameSymbol,

    -- ** Data types
    XName (unXName),
    SXName (..),
    SomeXName (..),
    xNameVal,
    someXNameVal,

    -- * XNameLower
    XNameLowerSymbol,
    IsXNameLowerSymbol,
    sIsXNameLowerSymbol,
    testXNameLowerSymbol,

    -- * XNameUpper
    XNameUpperSymbol,
    IsXNameUpperSymbol,
    sIsXNameUpperSymbol,
    testXNameUpperSymbol,
  )
where

import Data.Bool.Singletons (SBool (SFalse, STrue), (%&&))
import Data.Constraint (Dict (..))
import Data.Kind (Constraint)
import Data.Maybe (isJust)
import Data.Ord.Singletons ((%>), type (>))
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Type.Bool (type (&&))
import GHC.TypeLits (KnownSymbol, Symbol)
import Text.Megaparsec (oneOf, takeWhile1P)
import Text.Megaparsec.Char (char)
import VCard.AlphaNumDash
  ( IsAlphaNumDashLowerSymbol,
    IsAlphaNumDashSymbol,
    IsAlphaNumDashUpperSymbol,
    sIsAlphaNumDashLowerSymbol,
    sIsAlphaNumDashSymbol,
    sIsAlphaNumDashUpperSymbol,
  )
import VCard.Char (isAlphaNumDashChar)
import VCard.Natural.Private (natSing)
import VCard.Parse (HasParser, Parser, parser)
import VCard.Serialize (HasSerializer, Serializer, serializer)
import VCard.Symbol.Private
  ( IsPrefixOf,
    IsPrefixOfInsensitive,
    Length,
    SSymbol,
    fromSSymbol,
    sIsPrefixOf,
    sIsPrefixOfInsensitive,
    sLength,
    symbolSing,
    testSSymbolEquality,
    withSomeSSymbol,
  )
import VCard.Util (Assert, NoInstance)

newtype XName = XName {unXName :: Text}
  deriving (Eq, Show, Ord)

instance HasParser XName where
  parser :: Parser XName
  parser = do
    x1 <- Text.singleton <$> oneOf ['x', 'X']
    x2 <- Text.singleton <$> char '-'
    x3 <- takeWhile1P (Just "AlphaNumDash") isAlphaNumDashChar

    pure $ XName $ Text.concat [x1, x2, x3]

instance HasSerializer XName where
  serializer :: Serializer XName
  serializer = unXName

data SXName s where
  SXName :: (XNameSymbol s) => SSymbol s -> SXName s

deriving instance Show (SXName s)

instance Eq (SXName s) where
  (==) _ _ = True

instance (KnownSymbol s) => HasParser (SXName s) where
  parser :: Parser (SXName s)
  parser = do
    ss <- parser @(SSymbol s)
    case testXNameSymbol ss of
      Nothing -> fail $ "No instance for (XNameSymbol " <> fromSSymbol ss <> ")"
      Just Dict -> pure (SXName ss)

instance HasSerializer (SXName s) where
  serializer :: Serializer (SXName s)
  serializer (SXName ss) = serializer ss

data SomeXName where
  SomeXName :: SXName s -> SomeXName

deriving instance Show SomeXName

instance Eq SomeXName where
  (SomeXName (SXName ss1)) == (SomeXName (SXName ss2)) =
    isJust (testSSymbolEquality ss1 ss2)

instance HasParser SomeXName where
  parser :: Parser SomeXName
  parser = someXNameVal <$> parser @XName

instance HasSerializer SomeXName where
  serializer :: Serializer SomeXName
  serializer (SomeXName sxname) = serializer sxname

xNameVal :: SXName s -> XName
xNameVal (SXName ss) = XName (serializer ss)

someXNameVal :: XName -> SomeXName
someXNameVal (XName t) =
  withSomeSSymbol (Text.unpack t) $ \ss ->
    case testXNameSymbol ss of
      Nothing -> error "panic: someXNameVal: invalid XName"
      Just Dict -> SomeXName (SXName ss)

-- Writing XNameSymbol/XNameSymbolLower/XNameSymbolUpper as type synonyms does
-- not work on GHC 9.2. Once we drop support for GHC 9.2 we can rewrite them as
-- type synonyms.
type family XNameSymbol (s :: Symbol) :: Constraint where
  XNameSymbol s = Assert (IsXNameSymbol s) (NoInstance "XNameSymbol" s)

testXNameSymbol :: SSymbol s -> Maybe (Dict (XNameSymbol s))
testXNameSymbol ss =
  case sIsXNameSymbol ss of
    STrue -> Just Dict
    SFalse -> Nothing

type IsXNameSymbol s =
  IsPrefixOfInsensitive "x-" s && Length s > 2 && IsAlphaNumDashSymbol s

sIsXNameSymbol :: SSymbol s -> SBool (IsXNameSymbol s)
sIsXNameSymbol ss =
  sIsPrefixOfInsensitive (symbolSing @"x-") ss
    %&& sLength ss %> natSing @2
    %&& sIsAlphaNumDashSymbol ss

type family XNameLowerSymbol (s :: Symbol) :: Constraint where
  XNameLowerSymbol s =
    Assert (IsXNameLowerSymbol s) (NoInstance "XNameLowerSymbol" s)

testXNameLowerSymbol :: SSymbol s -> Maybe (Dict (XNameLowerSymbol s))
testXNameLowerSymbol ss =
  case sIsXNameLowerSymbol ss of
    STrue -> Just Dict
    SFalse -> Nothing

type IsXNameLowerSymbol s =
  IsPrefixOf "x-" s && Length s > 2 && IsAlphaNumDashLowerSymbol s

sIsXNameLowerSymbol :: SSymbol s -> SBool (IsXNameLowerSymbol s)
sIsXNameLowerSymbol ss =
  sIsPrefixOf (symbolSing @"x-") ss
    %&& sLength ss %> natSing @2
    %&& sIsAlphaNumDashLowerSymbol ss

type family XNameUpperSymbol (s :: Symbol) :: Constraint where
  XNameUpperSymbol s =
    Assert (IsXNameUpperSymbol s) (NoInstance "XNameUpperSymbol" s)

testXNameUpperSymbol :: SSymbol s -> Maybe (Dict (XNameUpperSymbol s))
testXNameUpperSymbol ss =
  case sIsXNameUpperSymbol ss of
    STrue -> Just Dict
    SFalse -> Nothing

type IsXNameUpperSymbol s =
  IsPrefixOf "X-" s && Length s > 2 && IsAlphaNumDashUpperSymbol s

sIsXNameUpperSymbol :: SSymbol s -> SBool (IsXNameUpperSymbol s)
sIsXNameUpperSymbol ss =
  sIsPrefixOf (symbolSing @"X-") ss
    %&& sLength ss %> natSing @2
    %&& sIsAlphaNumDashUpperSymbol ss
