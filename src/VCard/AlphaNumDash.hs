-- SPDX-FileCopyrightText: Copyright Preetham Gujjula
-- SPDX-License-Identifier: BSD-3-Clause
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module VCard.AlphaNumDash
  ( AlphaNumDashSymbol,
    testAlphaNumDashSymbol,
    IsAlphaNumDashSymbol,
    sIsAlphaNumDashSymbol,
    --
    AlphaNumDashLowerSymbol,
    testAlphaNumDashLowerSymbol,
    IsAlphaNumDashLowerSymbol,
    sIsAlphaNumDashLowerSymbol,
    --
    AlphaNumDashUpperSymbol,
    testAlphaNumDashUpperSymbol,
    IsAlphaNumDashUpperSymbol,
    sIsAlphaNumDashUpperSymbol,
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
import GHC.TypeLits (Symbol)
import Unsafe.Coerce (unsafeCoerce)
import VCard.Natural.Private (natSing)
import VCard.Symbol.Private
  ( Length,
    SChar,
    SSymbol,
    ToList,
    charSing,
    sLength,
    sToList,
    testSCharEquality,
  )
import VCard.Util (NoInstance, Truth)

-- Writing AlphaNumDashSymbol/AlphaNumDashLowerSymbol/
-- AlphaNumDashUpperSymbol as type synonyms does not work on GHC 9.2. Once we
-- drop support for GHC 9.2 we can rewrite them as type synonyms.
type family AlphaNumDashSymbol (s :: Symbol) :: Constraint where
  AlphaNumDashSymbol s =
    If (IsAlphaNumDashSymbol s) Truth (NoInstance "AlphaNumDashSymbol" s)

testAlphaNumDashSymbol :: SSymbol s -> Maybe (Dict (AlphaNumDashSymbol s))
testAlphaNumDashSymbol ss =
  case sIsAlphaNumDashSymbol ss of
    STrue -> Just Dict
    SFalse -> Nothing

type IsAlphaNumDashSymbol s = Length s > 0 && IsAlphaNumDashList (ToList s)

sIsAlphaNumDashSymbol :: SSymbol s -> SBool (IsAlphaNumDashSymbol s)
sIsAlphaNumDashSymbol ss =
  sLength ss %> natSing @0 %&& sIsAlphaNumDashList (sToList ss)

type family AlphaNumDashLowerSymbol (s :: Symbol) :: Constraint where
  AlphaNumDashLowerSymbol s =
    If
      (IsAlphaNumDashLowerSymbol s)
      Truth
      (NoInstance "AlphaNumDashLowerSymbol" s)

testAlphaNumDashLowerSymbol ::
  SSymbol s -> Maybe (Dict (AlphaNumDashLowerSymbol s))
testAlphaNumDashLowerSymbol ss =
  case sIsAlphaNumDashLowerSymbol ss of
    STrue -> Just Dict
    SFalse -> Nothing

type IsAlphaNumDashLowerSymbol s =
  Length s > 0 && IsAlphaNumDashLowerList (ToList s)

sIsAlphaNumDashLowerSymbol ::
  SSymbol s -> SBool (IsAlphaNumDashLowerSymbol s)
sIsAlphaNumDashLowerSymbol ss =
  sLength ss %> natSing @0 %&& sIsAlphaNumDashLowerList (sToList ss)

type family AlphaNumDashUpperSymbol (s :: Symbol) :: Constraint where
  AlphaNumDashUpperSymbol s =
    If
      (IsAlphaNumDashUpperSymbol s)
      Truth
      (NoInstance "AlphaNumDashUpperSymbol" s)

testAlphaNumDashUpperSymbol ::
  SSymbol s -> Maybe (Dict (AlphaNumDashUpperSymbol s))
testAlphaNumDashUpperSymbol ss =
  case sIsAlphaNumDashUpperSymbol ss of
    STrue -> Just Dict
    SFalse -> Nothing

type IsAlphaNumDashUpperSymbol s =
  Length s > 0 && IsAlphaNumDashUpperList (ToList s)

sIsAlphaNumDashUpperSymbol ::
  SSymbol s -> SBool (IsAlphaNumDashUpperSymbol s)
sIsAlphaNumDashUpperSymbol ss =
  sLength ss %> natSing @0 %&& sIsAlphaNumDashUpperList (sToList ss)

type family IsAlphaNumDashList (xs :: [Char]) where
  IsAlphaNumDashList '[] = True
  IsAlphaNumDashList (x : xs) =
    IsAlphaNumDashChar x && IsAlphaNumDashList xs

sIsAlphaNumDashList :: SList (xs :: [Char]) -> SBool (IsAlphaNumDashList xs)
sIsAlphaNumDashList SNil = STrue
sIsAlphaNumDashList (SCons sx sxs) =
  sIsAlphaNumDashChar sx %&& sIsAlphaNumDashList sxs

type family IsAlphaNumDashLowerList (xs :: [Char]) where
  IsAlphaNumDashLowerList '[] = True
  IsAlphaNumDashLowerList (x : xs) =
    IsAlphaNumDashLowerChar x && IsAlphaNumDashLowerList xs

sIsAlphaNumDashLowerList ::
  SList (xs :: [Char]) -> SBool (IsAlphaNumDashLowerList xs)
sIsAlphaNumDashLowerList SNil = STrue
sIsAlphaNumDashLowerList (SCons sx sxs) =
  sIsAlphaNumDashLowerChar sx %&& sIsAlphaNumDashLowerList sxs

type family IsAlphaNumDashUpperList (xs :: [Char]) where
  IsAlphaNumDashUpperList '[] = True
  IsAlphaNumDashUpperList (x : xs) =
    IsAlphaNumDashUpperChar x && IsAlphaNumDashUpperList xs

sIsAlphaNumDashUpperList ::
  SList (xs :: [Char]) -> SBool (IsAlphaNumDashUpperList xs)
sIsAlphaNumDashUpperList SNil = STrue
sIsAlphaNumDashUpperList (SCons sx sxs) =
  sIsAlphaNumDashUpperChar sx %&& sIsAlphaNumDashUpperList sxs

type IsAlphaNumDashChar c = IsAsciiAlpha c || IsDigit c || c == '-'

sIsAlphaNumDashChar :: SChar c -> SBool (IsAlphaNumDashChar c)
sIsAlphaNumDashChar sc =
  sIsAsciiAlpha sc %|| sIsDigit sc %|| sc %== charSing @'-'

type IsAlphaNumDashLowerChar c = IsAsciiLower c || IsDigit c || c == '-'

sIsAlphaNumDashLowerChar :: SChar c -> SBool (IsAlphaNumDashLowerChar c)
sIsAlphaNumDashLowerChar sc =
  sIsAsciiLower sc %|| sIsDigit sc %|| sc %== charSing @'-'

type IsAlphaNumDashUpperChar c = IsAsciiUpper c || IsDigit c || c == '-'

sIsAlphaNumDashUpperChar :: SChar c -> SBool (IsAlphaNumDashUpperChar c)
sIsAlphaNumDashUpperChar sc =
  sIsAsciiUpper sc %|| sIsDigit sc %|| sc %== charSing @'-'

type IsAsciiAlpha c = IsAsciiLower c || IsAsciiUpper c

sIsAsciiAlpha :: SChar c -> SBool (IsAsciiAlpha c)
sIsAsciiAlpha sc = sIsAsciiLower sc %|| sIsAsciiUpper sc

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
