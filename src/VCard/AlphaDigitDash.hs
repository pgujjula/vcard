-- SPDX-FileCopyrightText: Copyright Preetham Gujjula
-- SPDX-License-Identifier: BSD-3-Clause
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module VCard.AlphaDigitDash
  ( AlphaDigitDashSymbol,
    SomeAlphaDigitDashSymbol (..),
    testAlphaDigitDashSymbol,
    IsAlphaDigitDashSymbol,
    sIsAlphaDigitDashSymbol,
    --
    AlphaDigitDashLowerSymbol,
    SomeAlphaDigitDashLowerSymbol (..),
    testAlphaDigitDashLowerSymbol,
    IsAlphaDigitDashLowerSymbol,
    sIsAlphaDigitDashLowerSymbol,
    --
    AlphaDigitDashUpperSymbol,
    SomeAlphaDigitDashUpperSymbol (..),
    testAlphaDigitDashUpperSymbol,
    IsAlphaDigitDashUpperSymbol,
    sIsAlphaDigitDashUpperSymbol,
  )
where

import Data.Bool.Singletons (SBool (SFalse, STrue), (%&&), (%||))
import Data.Char (isAsciiLower, isAsciiUpper, isDigit)
import Data.Constraint (Dict (..))
import Data.Eq.Singletons ((%==), type (==))
import Data.Kind (Constraint)
import Data.List.Singletons (SList (SCons, SNil))
import Data.Maybe (isJust)
import Data.Ord.Singletons ((%>), type (>))
import Data.Text qualified as Text
import Data.Type.Bool (If, type (&&), type (||))
import Data.Type.Equality ((:~:) (Refl))
import GHC.TypeLits (Symbol, symbolVal)
import Text.Megaparsec (takeWhile1P)
import Unsafe.Coerce (unsafeCoerce)
import VCard.Natural.Private (natSing)
import VCard.Parse (HasParser, Parser, parser)
import VCard.Serialize (HasSerializer, Serializer, serializer)
import VCard.Symbol.Private
  ( Length,
    SChar,
    SSymbol,
    ToList,
    charSing,
    sLength,
    sToList,
    testSCharEquality,
    testSSymbolEquality,
    withKnownSymbol,
    withSomeSSymbol,
  )
import VCard.Util (NoInstance, Truth)

-- Writing AlphaDigitDashSymbol/AlphaDigitDashLowerSymbol/
-- AlphaDigitDashUpperSymbol as type synonyms does not work on GHC 9.2. Once we
-- drop support for GHC 9.2 we can rewrite them as type synonyms.
type family AlphaDigitDashSymbol (s :: Symbol) :: Constraint where
  AlphaDigitDashSymbol s =
    If (IsAlphaDigitDashSymbol s) Truth (NoInstance "AlphaDigitDashSymbol" s)

data SomeAlphaDigitDashSymbol where
  SomeAlphaDigitDashSymbol ::
    (AlphaDigitDashSymbol s) => SSymbol s -> SomeAlphaDigitDashSymbol

deriving instance Show SomeAlphaDigitDashSymbol

instance Eq SomeAlphaDigitDashSymbol where
  (==)
    (SomeAlphaDigitDashSymbol ss1)
    (SomeAlphaDigitDashSymbol ss2) =
      isJust (testSSymbolEquality ss1 ss2)

instance HasParser SomeAlphaDigitDashSymbol where
  parser :: Parser SomeAlphaDigitDashSymbol
  parser = do
    s <- takeWhile1P (Just "SomeAlphaDigitDashSymbol") isAlphaDigitDashChar
    withSomeSSymbol (Text.unpack s) $ \ss ->
      case testAlphaDigitDashSymbol ss of
        Nothing ->
          error "parser @SomeAlphaDigitDashSymbol: the impossible happened"
        Just Dict -> pure (SomeAlphaDigitDashSymbol ss)

instance HasSerializer SomeAlphaDigitDashSymbol where
  serializer :: Serializer SomeAlphaDigitDashSymbol
  serializer (SomeAlphaDigitDashSymbol ss) =
    Text.pack (withKnownSymbol ss (symbolVal ss))

testAlphaDigitDashSymbol :: SSymbol s -> Maybe (Dict (AlphaDigitDashSymbol s))
testAlphaDigitDashSymbol ss =
  case sIsAlphaDigitDashSymbol ss of
    STrue -> Just Dict
    SFalse -> Nothing

type IsAlphaDigitDashSymbol s = Length s > 0 && IsAlphaDigitDashList (ToList s)

sIsAlphaDigitDashSymbol :: SSymbol s -> SBool (IsAlphaDigitDashSymbol s)
sIsAlphaDigitDashSymbol ss =
  sLength ss %> natSing @0 %&& sIsAlphaDigitDashList (sToList ss)

type family AlphaDigitDashLowerSymbol (s :: Symbol) :: Constraint where
  AlphaDigitDashLowerSymbol s =
    If
      (IsAlphaDigitDashLowerSymbol s)
      Truth
      (NoInstance "AlphaDigitDashLowerSymbol" s)

data SomeAlphaDigitDashLowerSymbol where
  SomeAlphaDigitDashLowerSymbol ::
    (AlphaDigitDashLowerSymbol s) => SSymbol s -> SomeAlphaDigitDashLowerSymbol

deriving instance Show SomeAlphaDigitDashLowerSymbol

instance Eq SomeAlphaDigitDashLowerSymbol where
  (==)
    (SomeAlphaDigitDashLowerSymbol ss1)
    (SomeAlphaDigitDashLowerSymbol ss2) =
      isJust (testSSymbolEquality ss1 ss2)

instance HasParser SomeAlphaDigitDashLowerSymbol where
  parser :: Parser SomeAlphaDigitDashLowerSymbol
  parser = do
    s <-
      takeWhile1P
        (Just "SomeAlphaDigitDashLowerSymbol")
        isAlphaDigitDashLowerChar
    withSomeSSymbol (Text.unpack s) $ \ss ->
      case testAlphaDigitDashLowerSymbol ss of
        Nothing ->
          error "parser @SomeAlphaDigitDashLowerSymbol: the impossible happened"
        Just Dict -> pure (SomeAlphaDigitDashLowerSymbol ss)

instance HasSerializer SomeAlphaDigitDashLowerSymbol where
  serializer :: Serializer SomeAlphaDigitDashLowerSymbol
  serializer (SomeAlphaDigitDashLowerSymbol ss) =
    Text.pack (withKnownSymbol ss (symbolVal ss))

testAlphaDigitDashLowerSymbol ::
  SSymbol s -> Maybe (Dict (AlphaDigitDashLowerSymbol s))
testAlphaDigitDashLowerSymbol ss =
  case sIsAlphaDigitDashLowerSymbol ss of
    STrue -> Just Dict
    SFalse -> Nothing

type IsAlphaDigitDashLowerSymbol s =
  Length s > 0 && IsAlphaDigitDashLowerList (ToList s)

sIsAlphaDigitDashLowerSymbol ::
  SSymbol s -> SBool (IsAlphaDigitDashLowerSymbol s)
sIsAlphaDigitDashLowerSymbol ss =
  sLength ss %> natSing @0 %&& sIsAlphaDigitDashLowerList (sToList ss)

type family AlphaDigitDashUpperSymbol (s :: Symbol) :: Constraint where
  AlphaDigitDashUpperSymbol s =
    If
      (IsAlphaDigitDashUpperSymbol s)
      Truth
      (NoInstance "AlphaDigitDashUpperSymbol" s)

data SomeAlphaDigitDashUpperSymbol where
  SomeAlphaDigitDashUpperSymbol ::
    (AlphaDigitDashUpperSymbol s) => SSymbol s -> SomeAlphaDigitDashUpperSymbol

deriving instance Show SomeAlphaDigitDashUpperSymbol

instance Eq SomeAlphaDigitDashUpperSymbol where
  (==)
    (SomeAlphaDigitDashUpperSymbol ss1)
    (SomeAlphaDigitDashUpperSymbol ss2) =
      isJust (testSSymbolEquality ss1 ss2)

instance HasParser SomeAlphaDigitDashUpperSymbol where
  parser :: Parser SomeAlphaDigitDashUpperSymbol
  parser = do
    s <-
      takeWhile1P
        (Just "SomeAlphaDigitDashUpperSymbol")
        isAlphaDigitDashUpperChar
    withSomeSSymbol (Text.unpack s) $ \ss ->
      case testAlphaDigitDashUpperSymbol ss of
        Nothing ->
          error "parser @SomeAlphaDigitDashUpperSymbol: the impossible happened"
        Just Dict -> pure (SomeAlphaDigitDashUpperSymbol ss)

instance HasSerializer SomeAlphaDigitDashUpperSymbol where
  serializer :: Serializer SomeAlphaDigitDashUpperSymbol
  serializer (SomeAlphaDigitDashUpperSymbol ss) =
    Text.pack (withKnownSymbol ss (symbolVal ss))

testAlphaDigitDashUpperSymbol ::
  SSymbol s -> Maybe (Dict (AlphaDigitDashUpperSymbol s))
testAlphaDigitDashUpperSymbol ss =
  case sIsAlphaDigitDashUpperSymbol ss of
    STrue -> Just Dict
    SFalse -> Nothing

type IsAlphaDigitDashUpperSymbol s =
  Length s > 0 && IsAlphaDigitDashUpperList (ToList s)

sIsAlphaDigitDashUpperSymbol ::
  SSymbol s -> SBool (IsAlphaDigitDashUpperSymbol s)
sIsAlphaDigitDashUpperSymbol ss =
  sLength ss %> natSing @0 %&& sIsAlphaDigitDashUpperList (sToList ss)

type family IsAlphaDigitDashList (xs :: [Char]) where
  IsAlphaDigitDashList '[] = True
  IsAlphaDigitDashList (x : xs) =
    IsAlphaDigitDashChar x && IsAlphaDigitDashList xs

sIsAlphaDigitDashList :: SList (xs :: [Char]) -> SBool (IsAlphaDigitDashList xs)
sIsAlphaDigitDashList SNil = STrue
sIsAlphaDigitDashList (SCons sx sxs) =
  sIsAlphaDigitDashChar sx %&& sIsAlphaDigitDashList sxs

type family IsAlphaDigitDashLowerList (xs :: [Char]) where
  IsAlphaDigitDashLowerList '[] = True
  IsAlphaDigitDashLowerList (x : xs) =
    IsAlphaDigitDashLowerChar x && IsAlphaDigitDashLowerList xs

sIsAlphaDigitDashLowerList ::
  SList (xs :: [Char]) -> SBool (IsAlphaDigitDashLowerList xs)
sIsAlphaDigitDashLowerList SNil = STrue
sIsAlphaDigitDashLowerList (SCons sx sxs) =
  sIsAlphaDigitDashLowerChar sx %&& sIsAlphaDigitDashLowerList sxs

type family IsAlphaDigitDashUpperList (xs :: [Char]) where
  IsAlphaDigitDashUpperList '[] = True
  IsAlphaDigitDashUpperList (x : xs) =
    IsAlphaDigitDashUpperChar x && IsAlphaDigitDashUpperList xs

sIsAlphaDigitDashUpperList ::
  SList (xs :: [Char]) -> SBool (IsAlphaDigitDashUpperList xs)
sIsAlphaDigitDashUpperList SNil = STrue
sIsAlphaDigitDashUpperList (SCons sx sxs) =
  sIsAlphaDigitDashUpperChar sx %&& sIsAlphaDigitDashUpperList sxs

type IsAlphaDigitDashChar c = IsAsciiAlpha c || IsDigit c || c == '-'

sIsAlphaDigitDashChar :: SChar c -> SBool (IsAlphaDigitDashChar c)
sIsAlphaDigitDashChar sc =
  sIsAsciiAlpha sc %|| sIsDigit sc %|| sc %== charSing @'-'

isAlphaDigitDashChar :: Char -> Bool
isAlphaDigitDashChar c = isAsciiAlpha c || isDigit c || c == '-'

type IsAlphaDigitDashLowerChar c = IsAsciiLower c || IsDigit c || c == '-'

sIsAlphaDigitDashLowerChar :: SChar c -> SBool (IsAlphaDigitDashLowerChar c)
sIsAlphaDigitDashLowerChar sc =
  sIsAsciiLower sc %|| sIsDigit sc %|| sc %== charSing @'-'

isAlphaDigitDashLowerChar :: Char -> Bool
isAlphaDigitDashLowerChar c = isAsciiLower c || isDigit c || c == '-'

type IsAlphaDigitDashUpperChar c = IsAsciiUpper c || IsDigit c || c == '-'

sIsAlphaDigitDashUpperChar :: SChar c -> SBool (IsAlphaDigitDashUpperChar c)
sIsAlphaDigitDashUpperChar sc =
  sIsAsciiUpper sc %|| sIsDigit sc %|| sc %== charSing @'-'

isAlphaDigitDashUpperChar :: Char -> Bool
isAlphaDigitDashUpperChar c = isAsciiUpper c || isDigit c || c == '-'

type IsAsciiAlpha c = IsAsciiLower c || IsAsciiUpper c

sIsAsciiAlpha :: SChar c -> SBool (IsAsciiAlpha c)
sIsAsciiAlpha sc = sIsAsciiLower sc %|| sIsAsciiUpper sc

isAsciiAlpha :: Char -> Bool
isAsciiAlpha c = isAsciiLower c || isAsciiUpper c

type family IsAsciiUpper c where
  IsAsciiUpper 'A' = True
  IsAsciiUpper 'B' = True
  IsAsciiUpper 'C' = True
  IsAsciiUpper 'D' = True
  IsAsciiUpper 'E' = True
  IsAsciiUpper 'F' = True
  IsAsciiUpper 'G' = True
  IsAsciiUpper 'H' = True
  IsAsciiUpper 'I' = True
  IsAsciiUpper 'J' = True
  IsAsciiUpper 'K' = True
  IsAsciiUpper 'L' = True
  IsAsciiUpper 'M' = True
  IsAsciiUpper 'N' = True
  IsAsciiUpper 'O' = True
  IsAsciiUpper 'P' = True
  IsAsciiUpper 'Q' = True
  IsAsciiUpper 'R' = True
  IsAsciiUpper 'S' = True
  IsAsciiUpper 'T' = True
  IsAsciiUpper 'U' = True
  IsAsciiUpper 'V' = True
  IsAsciiUpper 'W' = True
  IsAsciiUpper 'X' = True
  IsAsciiUpper 'Y' = True
  IsAsciiUpper 'Z' = True
  IsAsciiUpper c = False

sIsAsciiUpper :: forall c. SChar c -> SBool (IsAsciiUpper c)
sIsAsciiUpper sc =
  markUpper (charSing @'A')
    . markUpper (charSing @'B')
    . markUpper (charSing @'C')
    . markUpper (charSing @'D')
    . markUpper (charSing @'E')
    . markUpper (charSing @'F')
    . markUpper (charSing @'G')
    . markUpper (charSing @'H')
    . markUpper (charSing @'I')
    . markUpper (charSing @'J')
    . markUpper (charSing @'K')
    . markUpper (charSing @'L')
    . markUpper (charSing @'M')
    . markUpper (charSing @'N')
    . markUpper (charSing @'O')
    . markUpper (charSing @'P')
    . markUpper (charSing @'Q')
    . markUpper (charSing @'R')
    . markUpper (charSing @'S')
    . markUpper (charSing @'T')
    . markUpper (charSing @'U')
    . markUpper (charSing @'V')
    . markUpper (charSing @'W')
    . markUpper (charSing @'X')
    . markUpper (charSing @'Y')
    . markUpper (charSing @'Z')
    $ unsafeCoerce SFalse
  where
    markUpper ::
      (IsAsciiUpper x ~ True) =>
      SChar x ->
      SBool (IsAsciiUpper c) ->
      SBool (IsAsciiUpper c)
    markUpper sx sb = maybe sb (\Refl -> STrue) (testSCharEquality sc sx)

type family IsAsciiLower c where
  IsAsciiLower 'a' = True
  IsAsciiLower 'b' = True
  IsAsciiLower 'c' = True
  IsAsciiLower 'd' = True
  IsAsciiLower 'e' = True
  IsAsciiLower 'f' = True
  IsAsciiLower 'g' = True
  IsAsciiLower 'h' = True
  IsAsciiLower 'i' = True
  IsAsciiLower 'j' = True
  IsAsciiLower 'k' = True
  IsAsciiLower 'l' = True
  IsAsciiLower 'm' = True
  IsAsciiLower 'n' = True
  IsAsciiLower 'o' = True
  IsAsciiLower 'p' = True
  IsAsciiLower 'q' = True
  IsAsciiLower 'r' = True
  IsAsciiLower 's' = True
  IsAsciiLower 't' = True
  IsAsciiLower 'u' = True
  IsAsciiLower 'v' = True
  IsAsciiLower 'w' = True
  IsAsciiLower 'x' = True
  IsAsciiLower 'y' = True
  IsAsciiLower 'z' = True
  IsAsciiLower c = False

sIsAsciiLower :: forall c. SChar c -> SBool (IsAsciiLower c)
sIsAsciiLower sc =
  markLower (charSing @'a')
    . markLower (charSing @'b')
    . markLower (charSing @'c')
    . markLower (charSing @'d')
    . markLower (charSing @'e')
    . markLower (charSing @'f')
    . markLower (charSing @'g')
    . markLower (charSing @'h')
    . markLower (charSing @'i')
    . markLower (charSing @'j')
    . markLower (charSing @'k')
    . markLower (charSing @'l')
    . markLower (charSing @'m')
    . markLower (charSing @'n')
    . markLower (charSing @'o')
    . markLower (charSing @'p')
    . markLower (charSing @'q')
    . markLower (charSing @'r')
    . markLower (charSing @'s')
    . markLower (charSing @'t')
    . markLower (charSing @'u')
    . markLower (charSing @'v')
    . markLower (charSing @'w')
    . markLower (charSing @'x')
    . markLower (charSing @'y')
    . markLower (charSing @'z')
    $ unsafeCoerce SFalse
  where
    markLower ::
      (IsAsciiLower x ~ True) =>
      SChar x ->
      SBool (IsAsciiLower c) ->
      SBool (IsAsciiLower c)
    markLower sx sb = maybe sb (\Refl -> STrue) (testSCharEquality sc sx)

type family IsDigit c where
  IsDigit '0' = True
  IsDigit '1' = True
  IsDigit '2' = True
  IsDigit '3' = True
  IsDigit '4' = True
  IsDigit '5' = True
  IsDigit '6' = True
  IsDigit '7' = True
  IsDigit '8' = True
  IsDigit '9' = True
  IsDigit c = False

sIsDigit :: forall c. SChar c -> SBool (IsDigit c)
sIsDigit sc =
  markDigit (charSing @'0')
    . markDigit (charSing @'1')
    . markDigit (charSing @'2')
    . markDigit (charSing @'3')
    . markDigit (charSing @'4')
    . markDigit (charSing @'5')
    . markDigit (charSing @'6')
    . markDigit (charSing @'7')
    . markDigit (charSing @'8')
    . markDigit (charSing @'9')
    $ unsafeCoerce SFalse
  where
    markDigit ::
      forall x.
      (IsDigit x ~ True) =>
      SChar x ->
      SBool (IsDigit c) ->
      SBool (IsDigit c)
    markDigit sx sb = maybe sb (\Refl -> STrue) (testSCharEquality sc sx)
