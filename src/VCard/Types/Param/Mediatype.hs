-- SPDX-FileCopyrightText: Copyright Preetham Gujjula
-- SPDX-License-Identifier: BSD-3-Clause
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module VCard.Types.Param.Mediatype
  ( MediatypeParam,
    Mediatype (..),

    -- * Type name
    TypeName,
    TypeNameSymbol,
    unTypeName,
    typeName,
    typeNameMaybe,

    -- * Subtype name
    SubtypeName,
    SubtypeNameSymbol,
    unSubtypeName,
    subtypeName,
    subtypeNameMaybe,

    -- * Parameter
    Parameter (..),

    -- ** Parameter Attribute
    Attribute,
    AttributeSymbol,
    unAttribute,
    attribute,
    attributeMaybe,

    -- ** Parameter Value
    Value,
    ValueSymbol,
    unValue,
    value,
    valueMaybe,
  )
where

import Data.Bool.Singletons
  ( If,
    Not,
    SBool (SFalse, STrue),
    sIf,
    sNot,
    (%&&),
    (%||),
    type (&&),
    type (||),
  )
import Data.Char (isAscii, isControl)
import Data.Constraint (Constraint, Dict (..))
import Data.Eq.Singletons ((%/=), (%==), type (/=), type (==))
import Data.List.Singletons (Elem, SList (SCons, SNil), sElem)
import Data.Ord.Singletons ((%<=), type (<=))
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as Text
import GHC.TypeLits (Symbol)
import Text.Megaparsec (choice, many, satisfy, takeWhile1P, try)
import Text.Megaparsec.Char (char)
import VCard.Char
  ( DQuote,
    IsAscii,
    IsAsciiAlpha,
    IsDigit,
    Space,
    dQuote,
    sDQuote,
    sIsAscii,
    sIsAsciiAlpha,
    sIsDigit,
    sSpace,
  )
import VCard.Parse (HasParser, Parser, parser)
import VCard.Serialize (HasSerializer, Serializer, serializer)
import VCard.Types.Param.Generic (GenericParam)
import VCard.Util (Assert, NoInstance)
import VCard.Util.Natural (natSing)
import VCard.Util.Symbol
  ( Length,
    SChar,
    SSymbol,
    ToList,
    charSing,
    sLength,
    sToList,
    symbolSing,
    withSomeSSymbol,
  )

type MediatypeParam = GenericParam "MEDIATYPE" Mediatype

data Mediatype = Mediatype
  { mTypeName :: TypeName,
    mSubtypeName :: SubtypeName,
    mParameters :: [Parameter]
  }

newtype TypeName = TypeName {unTypeName :: Text}

type family TypeNameSymbol (s :: Symbol) :: Constraint where
  TypeNameSymbol s =
    Assert (IsTypeNameSymbol s) (NoInstance "TypeNameSymbol" s)

testTypeNameSymbol :: SSymbol s -> Maybe (Dict (TypeNameSymbol s))
testTypeNameSymbol ss =
  case sIsTypeNameSymbol ss of
    STrue -> Just Dict
    SFalse -> Nothing

data STypeName (s :: Symbol) where
  STypeName :: (TypeNameSymbol s) => SSymbol s -> STypeName s

typeName :: (TypeNameSymbol s) => SSymbol s -> TypeName
typeName ss = typeNameVal (STypeName ss)

typeNameMaybe :: Text -> Maybe TypeName
typeNameMaybe t =
  withSomeSSymbol (Text.unpack t) $ \ss ->
    case testTypeNameSymbol ss of
      Nothing -> Nothing
      Just Dict -> Just (typeName ss)

typeNameVal :: STypeName s -> TypeName
typeNameVal (STypeName ss) = TypeName (serializer ss)

type family IsTypeNameSymbol (s :: Symbol) :: Bool where
  IsTypeNameSymbol s = IsRegNameSymbol s

sIsTypeNameSymbol :: SSymbol s -> SBool (IsTypeNameSymbol s)
sIsTypeNameSymbol = sIsRegNameSymbol

newtype SubtypeName = SubtypeName {unSubtypeName :: Text}

subtypeNameVal :: SSubtypeName s -> SubtypeName
subtypeNameVal (SSubtypeName ss) = SubtypeName (serializer ss)

type family SubtypeNameSymbol (s :: Symbol) :: Constraint where
  SubtypeNameSymbol s =
    Assert (IsSubtypeNameSymbol s) (NoInstance "SubtypeNameSymbol" s)

data SSubtypeName s where
  SSubtypeName :: (SubtypeNameSymbol s) => SSymbol s -> SSubtypeName s

testSubtypeNameSymbol :: SSymbol s -> Maybe (Dict (SubtypeNameSymbol s))
testSubtypeNameSymbol ss =
  case sIsSubtypeNameSymbol ss of
    STrue -> Just Dict
    SFalse -> Nothing

subtypeName :: (SubtypeNameSymbol s) => SSymbol s -> SubtypeName
subtypeName ss = subtypeNameVal (SSubtypeName ss)

subtypeNameMaybe :: Text -> Maybe SubtypeName
subtypeNameMaybe t =
  withSomeSSymbol (Text.unpack t) $ \ss ->
    case testSubtypeNameSymbol ss of
      Nothing -> Nothing
      Just Dict -> Just (subtypeName ss)

type family IsSubtypeNameSymbol (s :: Symbol) :: Bool where
  IsSubtypeNameSymbol s = IsRegNameSymbol s

sIsSubtypeNameSymbol :: SSymbol s -> SBool (IsSubtypeNameSymbol s)
sIsSubtypeNameSymbol = sIsRegNameSymbol

--
-- Reg name
--
type family IsRegNameSymbol (s :: Symbol) :: Bool where
  IsRegNameSymbol s = s /= "" && Length s <= 127 && AllIsRegNameChar (ToList s)

sIsRegNameSymbol :: SSymbol s -> SBool (IsRegNameSymbol s)
sIsRegNameSymbol ss =
  ss %/= symbolSing @"" %&& sLength ss %<= natSing @127 %&& sAllIsRegNameChar (sToList ss)

type family AllIsRegNameChar (xs :: [Char]) :: Bool where
  AllIsRegNameChar '[] = True
  AllIsRegNameChar (x : xs) = IsRegNameChar x && AllIsRegNameChar xs

sAllIsRegNameChar :: SList (xs :: [Char]) -> SBool (AllIsRegNameChar xs)
sAllIsRegNameChar SNil = STrue
sAllIsRegNameChar (SCons sx sxs) = sIsRegNameChar sx %&& sAllIsRegNameChar sxs

type family IsRegNameChar (c :: Char) :: Bool where
  IsRegNameChar c =
    IsAsciiAlpha c
      || IsDigit c
      || Elem c ['!', '#', '$', '&', '.', '+', '-', '^', '_']

sIsRegNameChar :: SChar c -> SBool (IsRegNameChar c)
sIsRegNameChar sc =
  sIsAsciiAlpha sc
    %|| sIsDigit sc
    %|| sElem
      sc
      ( charSing @'!'
          `SCons` charSing @'#'
          `SCons` charSing @'$'
          `SCons` charSing @'&'
          `SCons` charSing @'.'
          `SCons` charSing @'+'
          `SCons` charSing @'-'
          `SCons` charSing @'^'
          `SCons` charSing @'_'
          `SCons` SNil
      )

--
-- Mediatype parameter
--
data Parameter = Parameter
  { pAttribute :: Attribute,
    pValue :: Value
  }

--
-- Mediatype parameter attribute
--
newtype Attribute = Attribute {unAttribute :: Text}

type family AttributeSymbol (s :: Symbol) :: Constraint where
  AttributeSymbol s =
    Assert (IsAttributeSymbol s) (NoInstance "AttributeSymbol" s)

data SAttribute s where
  SAttribute :: (AttributeSymbol s) => SSymbol s -> SAttribute s

deriving instance Show (SAttribute s)

type family IsAttributeSymbol (s :: Symbol) :: Bool where
  IsAttributeSymbol s = IsTokenSymbol s

sIsAttributeSymbol :: SSymbol s -> SBool (IsAttributeSymbol s)
sIsAttributeSymbol = sIsTokenSymbol

attributeVal :: SAttribute s -> Attribute
attributeVal (SAttribute ss) = Attribute (serializer ss)

testAttributeSymbol :: SSymbol s -> Maybe (Dict (AttributeSymbol s))
testAttributeSymbol ss =
  case sIsAttributeSymbol ss of
    STrue -> Just Dict
    SFalse -> Nothing

instance HasParser Attribute where
  parser :: Parser Attribute
  parser = Attribute <$> takeWhile1P (Just "Attribute") isTokenChar

instance HasSerializer Attribute where
  serializer :: Serializer Attribute
  serializer = unAttribute

attribute :: (AttributeSymbol s) => SSymbol s -> Attribute
attribute ss = attributeVal (SAttribute ss)

attributeMaybe :: Text -> Maybe Attribute
attributeMaybe t =
  withSomeSSymbol (Text.unpack t) $ \ss ->
    case testAttributeSymbol ss of
      Nothing -> Nothing
      Just Dict -> Just (attribute ss)

--
-- Mediatype parameter value
--
newtype Value = Value {unValue :: Text}

type family ValueSymbol (s :: Symbol) :: Constraint where
  ValueSymbol s =
    Assert (IsValueSymbol s) (NoInstance "ValueSymbol" s)

testValueSymbol :: SSymbol s -> Maybe (Dict (ValueSymbol s))
testValueSymbol ss =
  case sIsValueSymbol ss of
    STrue -> Just Dict
    SFalse -> Nothing

type family IsValueSymbol (s :: Symbol) :: Bool where
  IsValueSymbol s = IsTokenSymbol s || IsQuotedString s

sIsValueSymbol :: SSymbol s -> SBool (IsValueSymbol s)
sIsValueSymbol ss = sIsTokenSymbol ss %|| sIsQuotedString ss

instance HasParser Value where
  parser :: Parser Value
  parser =
    Value
      <$> choice
        [ try tokenP,
          quotedStringP
        ]

instance HasSerializer Value where
  serializer :: Serializer Value
  serializer = unValue

data SValue s where
  SValue :: (ValueSymbol s) => SSymbol s -> SValue s

deriving instance Show (SValue s)

data SomeValue where
  SomeValue :: SValue s -> SomeValue

deriving instance Show SomeValue

valueVal :: SValue s -> Value
valueVal (SValue ss) = Value (serializer ss)

value :: (ValueSymbol s) => SSymbol s -> Value
value ss = valueVal (SValue ss)

valueMaybe :: Text -> Maybe Value
valueMaybe t =
  withSomeSSymbol (Text.unpack t) $ \ss ->
    case testValueSymbol ss of
      Nothing -> Nothing
      Just Dict -> Just (value ss)

--
-- Token
--
type family IsTokenSymbol (s :: Symbol) :: Bool where
  IsTokenSymbol s = (s /= "") && AllIsTokenChar (ToList s)

sIsTokenSymbol :: SSymbol s -> SBool (IsTokenSymbol s)
sIsTokenSymbol ss = (ss %/= symbolSing @"") %&& sAllIsTokenChar (sToList ss)

type family AllIsTokenChar (xs :: [Char]) :: Bool where
  AllIsTokenChar '[] = True
  AllIsTokenChar (x : xs) = IsTokenChar x && AllIsTokenChar xs

sAllIsTokenChar :: SList (xs :: [Char]) -> SBool (AllIsTokenChar xs)
sAllIsTokenChar SNil = STrue
sAllIsTokenChar (SCons sx sxs) = sIsTokenChar sx %&& sAllIsTokenChar sxs

type family IsTokenChar (c :: Char) :: Bool where
  IsTokenChar c =
    IsAscii c && Not (c == Space || IsControlChar c || IsTSpecialChar c)

sIsTokenChar :: SChar c -> SBool (IsTokenChar c)
sIsTokenChar sc =
  sIsAscii sc
    %&& sNot (sc %== sSpace %|| sIsControlChar sc %|| sIsTSpecialChar sc)

type IsControlChar :: Char -> Bool
type IsControlChar c = c <= '\31' || c == '\127'

sIsControlChar :: SChar c -> SBool (IsControlChar c)
sIsControlChar sc = sc %<= charSing @'\31' %|| sc %== charSing @'\127'

type IsTSpecialChar :: Char -> Bool
type IsTSpecialChar c =
  Elem
    c
    ['(', ')', '<', '>', '@', ',', ';', ':', '\\', '"', '/', '[', ']', '?', '=']

sIsTSpecialChar :: SChar c -> SBool (IsTSpecialChar c)
sIsTSpecialChar sc =
  sElem
    sc
    ( charSing @'('
        `SCons` charSing @')'
        `SCons` charSing @'<'
        `SCons` charSing @'>'
        `SCons` charSing @'@'
        `SCons` charSing @','
        `SCons` charSing @';'
        `SCons` charSing @':'
        `SCons` charSing @'\\'
        `SCons` charSing @'"'
        `SCons` charSing @'/'
        `SCons` charSing @'['
        `SCons` charSing @']'
        `SCons` charSing @'?'
        `SCons` charSing @'='
        `SCons` SNil
    )

isTokenChar :: Char -> Bool
isTokenChar c = not (c == ' ' || isControl c || isTSpecial c)

isTSpecial :: Char -> Bool
isTSpecial c = Set.member c tSpecialSet

tSpecialSet :: Set Char
tSpecialSet =
  Set.fromList . concat $
    [ ['(', ')', '<', '>', '@'],
      [',', ';', ':', '\\', '"'],
      ['/', '[', ']', '?', '=']
    ]

tokenP :: Parser Text
tokenP = takeWhile1P (Just "Attribute") isTokenChar

--
-- Quoted string
--

type family IsQuotedString (s :: Symbol) :: Bool where
  IsQuotedString s = IsQuotedStringList (ToList s)

sIsQuotedString :: SSymbol s -> SBool (IsQuotedString s)
sIsQuotedString ss = sIsQuotedStringList (sToList ss)

type family IsQuotedStringList (xs :: [Char]) :: Bool where
  IsQuotedStringList '[] = False
  IsQuotedStringList (x : xs) = x == DQuote && IsQuotedStringTail xs

sIsQuotedStringList :: SList (xs :: [Char]) -> SBool (IsQuotedStringList xs)
sIsQuotedStringList SNil = SFalse
sIsQuotedStringList (SCons sx sxs) = sx %== sDQuote %&& sIsQuotedStringTail sxs

type family IsQuotedStringTail (xs :: [Char]) :: Bool where
  IsQuotedStringTail '[] = False
  IsQuotedStringTail (x : '[]) = x == DQuote
  IsQuotedStringTail (x1 : x2 : '[]) = IsQTextChar x1 && x2 == DQuote
  IsQuotedStringTail (x1 : x2 : x3 : xs) =
    If
      (IsQTextChar x1)
      (IsQuotedStringTail (x2 : x3 : xs))
      (IsQuotedPair x1 x2 && IsQuotedStringTail (x3 : xs))

sIsQuotedStringTail :: SList (xs :: [Char]) -> SBool (IsQuotedStringTail xs)
sIsQuotedStringTail SNil = SFalse
sIsQuotedStringTail (SCons sx SNil) = sx %== sDQuote
sIsQuotedStringTail (SCons sx1 (SCons sx2 SNil)) =
  sIsQTextChar sx1 %&& sx2 %== sDQuote
sIsQuotedStringTail (SCons sx1 (SCons sx2 (SCons sx3 sxs))) =
  sIf
    (sIsQTextChar sx1)
    (sIsQuotedStringTail (SCons sx2 (SCons sx3 sxs)))
    (sIsQuotedPair sx1 sx2 %&& sIsQuotedStringTail (SCons sx3 sxs))

type family IsQTextChar (c :: Char) :: Bool where
  IsQTextChar c = IsAscii c && Not (c == DQuote || c == '\\' || c == '\r')

sIsQTextChar :: SChar c -> SBool (IsQTextChar c)
sIsQTextChar sc =
  sIsAscii sc
    %&& sNot
      ( sc %== sDQuote
          %|| sc %== charSing @'\\'
          %|| sc %== charSing @'\r'
      )

type family IsQuotedPair (c1 :: Char) (c2 :: Char) :: Bool where
  IsQuotedPair c1 c2 = c1 == '\\' && IsAscii c2

sIsQuotedPair :: SChar c1 -> SChar c2 -> SBool (IsQuotedPair c1 c2)
sIsQuotedPair sc1 sc2 = sc1 %== charSing @'\\' %&& sIsAscii sc2

quotedStringP :: Parser Text
quotedStringP = char dQuote *> (Text.concat <$> many quotedCharP) <* char dQuote

quotedCharP :: Parser Text
quotedCharP =
  choice
    [ try qtextP,
      quotedPairP
    ]

qtextP :: Parser Text
qtextP =
  Text.singleton
    <$> satisfy (\c -> isAscii c && c `notElem` [dQuote, '\\', '\r'])

quotedPairP :: Parser Text
quotedPairP = do
  backslash <- char '\\'
  c <- satisfy isAscii
  pure $ Text.pack [backslash, c]
