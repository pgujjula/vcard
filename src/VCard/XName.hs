-- SPDX-FileCopyrightText: Copyright Preetham Gujjula
-- SPDX-License-Identifier: BSD-3-Clause
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module VCard.XName
  ( XNameSymbol,
    IsXNameSymbol,
    sIsXNameSymbol,
    testXNameSymbol,
    XNameLowerSymbol,
    IsXNameLowerSymbol,
    sIsXNameLowerSymbol,
    testXNameLowerSymbol,
    XNameUpperSymbol,
    IsXNameUpperSymbol,
    sIsXNameUpperSymbol,
    testXNameUpperSymbol,
  )
where

import Data.Bool.Singletons (SBool (SFalse, STrue), (%&&), (%||))
import Data.Constraint (Dict (..))
import Data.Eq.Singletons ((%==), type (==))
import Data.Kind (Constraint)
import Data.List.Singletons (SList (SCons, SNil))
import Data.Ord.Singletons ((%>), type (>))
import Data.Type.Bool (If, type (&&), type (||))
import Data.Type.Equality ((:~:) (Refl))
import GHC.TypeLits (ErrorMessage (ShowType, Text, (:<>:)), Symbol, TypeError)
import Unsafe.Coerce (unsafeCoerce)
import VCard.Natural.Private (natSing)
import VCard.Symbol.Private
  ( IsPrefixOf,
    IsPrefixOfInsensitive,
    Length,
    SChar,
    SSymbol,
    ToList,
    charSing,
    sIsPrefixOf,
    sIsPrefixOfInsensitive,
    sLength,
    sToList,
    symbolSing,
    testSCharEquality,
  )

type Valid :: Constraint
type Valid = ()

-- Writing XNameSymbol/XNameSymbolLower/XNameSymbolUpper as type synonyms does
-- not work on GHC 9.2. Once we drop support for GHC 9.2 we can rewrite them as
-- type synonyms.
type family XNameSymbol (s :: Symbol) :: Constraint where
  XNameSymbol s = If (IsXNameSymbol s) Valid (NoInstance "XNameSymbol" s)

testXNameSymbol :: SSymbol s -> Maybe (Dict (XNameSymbol s))
testXNameSymbol ss =
  case sIsXNameSymbol ss of
    STrue -> Just Dict
    SFalse -> Nothing

type IsXNameSymbol s =
  IsPrefixOfInsensitive "x-" s && Length s > 2 && AllIsXChar (ToList s)

sIsXNameSymbol :: SSymbol s -> SBool (IsXNameSymbol s)
sIsXNameSymbol ss =
  sIsPrefixOfInsensitive (symbolSing @"x-") ss
    %&& sLength ss %> natSing @2
    %&& sAllIsXChar (sToList ss)

type family XNameLowerSymbol (s :: Symbol) :: Constraint where
  XNameLowerSymbol s =
    If (IsXNameLowerSymbol s) Valid (NoInstance "XNameLowerSymbol" s)

testXNameLowerSymbol :: SSymbol s -> Maybe (Dict (XNameLowerSymbol s))
testXNameLowerSymbol ss =
  case sIsXNameLowerSymbol ss of
    STrue -> Just Dict
    SFalse -> Nothing

type IsXNameLowerSymbol s =
  IsPrefixOf "x-" s && Length s > 2 && AllIsXCharLower (ToList s)

sIsXNameLowerSymbol :: SSymbol s -> SBool (IsXNameLowerSymbol s)
sIsXNameLowerSymbol ss =
  sIsPrefixOf (symbolSing @"x-") ss
    %&& sLength ss %> natSing @2
    %&& sAllIsXCharLower (sToList ss)

type family XNameUpperSymbol (s :: Symbol) :: Constraint where
  XNameUpperSymbol s =
    If (IsXNameUpperSymbol s) Valid (NoInstance "XNameUpperSymbol" s)

testXNameUpperSymbol :: SSymbol s -> Maybe (Dict (XNameUpperSymbol s))
testXNameUpperSymbol ss =
  case sIsXNameUpperSymbol ss of
    STrue -> Just Dict
    SFalse -> Nothing

type IsXNameUpperSymbol s =
  IsPrefixOf "X-" s && Length s > 2 && AllIsXCharUpper (ToList s)

sIsXNameUpperSymbol :: SSymbol s -> SBool (IsXNameUpperSymbol s)
sIsXNameUpperSymbol ss =
  sIsPrefixOf (symbolSing @"X-") ss
    %&& sLength ss %> natSing @2
    %&& sAllIsXCharUpper (sToList ss)

type family AllIsXChar (xs :: [Char]) where
  AllIsXChar '[] = True
  AllIsXChar (x : xs) = IsXChar x && AllIsXChar xs

sAllIsXChar :: SList (xs :: [Char]) -> SBool (AllIsXChar xs)
sAllIsXChar SNil = STrue
sAllIsXChar (SCons sx sxs) = sIsXChar sx %&& sAllIsXChar sxs

type family AllIsXCharLower (xs :: [Char]) where
  AllIsXCharLower '[] = True
  AllIsXCharLower (x : xs) = IsXCharLower x && AllIsXCharLower xs

sAllIsXCharLower :: SList (xs :: [Char]) -> SBool (AllIsXCharLower xs)
sAllIsXCharLower SNil = STrue
sAllIsXCharLower (SCons sx sxs) = sIsXCharLower sx %&& sAllIsXCharLower sxs

type family AllIsXCharUpper (xs :: [Char]) where
  AllIsXCharUpper '[] = True
  AllIsXCharUpper (x : xs) = IsXCharUpper x && AllIsXCharUpper xs

sAllIsXCharUpper :: SList (xs :: [Char]) -> SBool (AllIsXCharUpper xs)
sAllIsXCharUpper SNil = STrue
sAllIsXCharUpper (SCons sx sxs) = sIsXCharUpper sx %&& sAllIsXCharUpper sxs

type IsXChar c = IsAlpha c || IsDigit c || c == '-'

sIsXChar :: SChar c -> SBool (IsXChar c)
sIsXChar sc = sIsAlpha sc %|| sIsDigit sc %|| sc %== charSing @'-'

type IsXCharLower c = IsAlphaLower c || IsDigit c || c == '-'

sIsXCharLower :: SChar c -> SBool (IsXCharLower c)
sIsXCharLower sc = sIsAlphaLower sc %|| sIsDigit sc %|| sc %== charSing @'-'

type IsXCharUpper c = IsAlphaUpper c || IsDigit c || c == '-'

sIsXCharUpper :: SChar c -> SBool (IsXCharUpper c)
sIsXCharUpper sc = sIsAlphaUpper sc %|| sIsDigit sc %|| sc %== charSing @'-'

type IsAlpha c = IsAlphaLower c || IsAlphaUpper c

sIsAlpha :: SChar c -> SBool (IsAlpha c)
sIsAlpha sc = sIsAlphaLower sc %|| sIsAlphaUpper sc

type family IsAlphaUpper c where
  IsAlphaUpper 'A' = True
  IsAlphaUpper 'B' = True
  IsAlphaUpper 'C' = True
  IsAlphaUpper 'D' = True
  IsAlphaUpper 'E' = True
  IsAlphaUpper 'F' = True
  IsAlphaUpper 'G' = True
  IsAlphaUpper 'H' = True
  IsAlphaUpper 'I' = True
  IsAlphaUpper 'J' = True
  IsAlphaUpper 'K' = True
  IsAlphaUpper 'L' = True
  IsAlphaUpper 'M' = True
  IsAlphaUpper 'N' = True
  IsAlphaUpper 'O' = True
  IsAlphaUpper 'P' = True
  IsAlphaUpper 'Q' = True
  IsAlphaUpper 'R' = True
  IsAlphaUpper 'S' = True
  IsAlphaUpper 'T' = True
  IsAlphaUpper 'U' = True
  IsAlphaUpper 'V' = True
  IsAlphaUpper 'W' = True
  IsAlphaUpper 'X' = True
  IsAlphaUpper 'Y' = True
  IsAlphaUpper 'Z' = True
  IsAlphaUpper c = False

sIsAlphaUpper :: forall c. SChar c -> SBool (IsAlphaUpper c)
sIsAlphaUpper sc =
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
      (IsAlphaUpper x ~ True) =>
      SChar x ->
      SBool (IsAlphaUpper c) ->
      SBool (IsAlphaUpper c)
    markUpper sx sb = maybe sb (\Refl -> STrue) (testSCharEquality sc sx)

type family IsAlphaLower c where
  IsAlphaLower 'a' = True
  IsAlphaLower 'b' = True
  IsAlphaLower 'c' = True
  IsAlphaLower 'd' = True
  IsAlphaLower 'e' = True
  IsAlphaLower 'f' = True
  IsAlphaLower 'g' = True
  IsAlphaLower 'h' = True
  IsAlphaLower 'i' = True
  IsAlphaLower 'j' = True
  IsAlphaLower 'k' = True
  IsAlphaLower 'l' = True
  IsAlphaLower 'm' = True
  IsAlphaLower 'n' = True
  IsAlphaLower 'o' = True
  IsAlphaLower 'p' = True
  IsAlphaLower 'q' = True
  IsAlphaLower 'r' = True
  IsAlphaLower 's' = True
  IsAlphaLower 't' = True
  IsAlphaLower 'u' = True
  IsAlphaLower 'v' = True
  IsAlphaLower 'w' = True
  IsAlphaLower 'x' = True
  IsAlphaLower 'y' = True
  IsAlphaLower 'z' = True
  IsAlphaLower c = False

sIsAlphaLower :: forall c. SChar c -> SBool (IsAlphaLower c)
sIsAlphaLower sc =
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
      (IsAlphaLower x ~ True) =>
      SChar x ->
      SBool (IsAlphaLower c) ->
      SBool (IsAlphaLower c)
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

-- Utilities
type family NoInstance (c :: Symbol) (s :: Symbol) where
  NoInstance c s =
    TypeError
      ( Text "No instance for ("
          :<>: Text c
          :<>: Text " "
          :<>: ShowType s
          :<>: Text ")"
      )
