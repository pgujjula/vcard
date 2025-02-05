-- SPDX-FileCopyrightText: Copyright Preetham Gujjula
-- SPDX-License-Identifier: BSD-3-Clause
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

-- | ⚠️ WARNING: This module is considered internal, and may change without prior
--   notice.
module VCard.Internal.Symbol
  ( -- * Change case

    -- ** Convert to lowercase
    ToLower,
    sToLower,
    ToLowerUncons,
    sToLowerUncons,
    ToLowerChar,
    sToLowerChar,

    -- ** Convert to uppercase
    ToUpper,
    sToUpper,
    ToUpperUncons,
    sToUpperUncons,
    ToUpperChar,
    sToUpperChar,

    -- * Prefix checking
    IsPrefixOf,
    sIsPrefixOf,
    IsPrefixOfInsensitive,
    sIsPrefixOfInsensitive,
    IsPrefixOfList,
    sIsPrefixOfList,

    -- * Convert to list
    ToList,
    sToList,
    ToListUncons,
    sToListUncons,

    -- * Miscellaneous singleton functions
    sConsSymbol,
    sUnconsSymbol,
  )
where

import Data.Bool.Singletons (SBool (SFalse, STrue), (%&&))
import Data.Eq.Singletons ((%==))
import Data.Kind (Constraint)
import Data.List.Singletons (SList (SCons, SNil))
import Data.Maybe.Singletons (SMaybe (SJust, SNothing))
import Data.Tuple.Singletons (STuple2 (..))
import Data.Type.Bool (type (&&))
import Data.Type.Equality (testEquality, (:~:) (Refl), type (==))
import GHC.TypeLits
  ( ConsSymbol,
    SChar,
    SSymbol,
    Symbol,
    UnconsSymbol,
    charSing,
    fromSChar,
    fromSSymbol,
    symbolSing,
    withSomeSChar,
    withSomeSSymbol,
  )
import Unsafe.Coerce (unsafeCoerce)

type family ToLower (s :: Symbol) :: Symbol where
  ToLower s = ToLowerUncons (UnconsSymbol s)

sToLower :: SSymbol s -> SSymbol (ToLower s)
sToLower ss = sToLowerUncons (sUnconsSymbol ss)

type family ToLowerUncons (x :: Maybe (Char, Symbol)) :: Symbol where
  ToLowerUncons Nothing = ""
  ToLowerUncons (Just '(c, s)) = ConsSymbol (ToLowerChar c) (ToLower s)

sToLowerUncons :: SMaybe (x :: Maybe (Char, Symbol)) -> SSymbol (ToLowerUncons x)
sToLowerUncons SNothing = symbolSing @""
sToLowerUncons (SJust (STuple2 sc ss)) = sConsSymbol (sToLowerChar sc) (sToLower ss)

type family ToLowerChar (a :: Char) :: Char where
  ToLowerChar 'A' = 'a'
  ToLowerChar 'B' = 'b'
  ToLowerChar 'C' = 'c'
  ToLowerChar 'D' = 'd'
  ToLowerChar 'E' = 'e'
  ToLowerChar 'F' = 'f'
  ToLowerChar 'G' = 'g'
  ToLowerChar 'H' = 'h'
  ToLowerChar 'I' = 'i'
  ToLowerChar 'J' = 'j'
  ToLowerChar 'K' = 'k'
  ToLowerChar 'L' = 'l'
  ToLowerChar 'M' = 'm'
  ToLowerChar 'N' = 'n'
  ToLowerChar 'O' = 'o'
  ToLowerChar 'P' = 'p'
  ToLowerChar 'Q' = 'q'
  ToLowerChar 'R' = 'r'
  ToLowerChar 'S' = 's'
  ToLowerChar 'T' = 't'
  ToLowerChar 'U' = 'u'
  ToLowerChar 'V' = 'v'
  ToLowerChar 'W' = 'w'
  ToLowerChar 'X' = 'x'
  ToLowerChar 'Y' = 'y'
  ToLowerChar 'Z' = 'z'
  ToLowerChar c = c

sToLowerChar :: SChar c -> SChar (ToLowerChar c)
sToLowerChar = sToLowerChar_

sToLowerChar_ :: forall c. SChar c -> SChar (ToLowerChar c)
sToLowerChar_ sc =
  handleCase (charSing @'A', \Refl -> charSing @'a')
    . handleCase (charSing @'B', \Refl -> charSing @'b')
    . handleCase (charSing @'C', \Refl -> charSing @'c')
    . handleCase (charSing @'D', \Refl -> charSing @'d')
    . handleCase (charSing @'E', \Refl -> charSing @'e')
    . handleCase (charSing @'F', \Refl -> charSing @'f')
    . handleCase (charSing @'G', \Refl -> charSing @'g')
    . handleCase (charSing @'H', \Refl -> charSing @'h')
    . handleCase (charSing @'I', \Refl -> charSing @'i')
    . handleCase (charSing @'J', \Refl -> charSing @'j')
    . handleCase (charSing @'K', \Refl -> charSing @'k')
    . handleCase (charSing @'L', \Refl -> charSing @'l')
    . handleCase (charSing @'M', \Refl -> charSing @'m')
    . handleCase (charSing @'N', \Refl -> charSing @'n')
    . handleCase (charSing @'O', \Refl -> charSing @'o')
    . handleCase (charSing @'P', \Refl -> charSing @'p')
    . handleCase (charSing @'Q', \Refl -> charSing @'q')
    . handleCase (charSing @'R', \Refl -> charSing @'r')
    . handleCase (charSing @'S', \Refl -> charSing @'s')
    . handleCase (charSing @'T', \Refl -> charSing @'t')
    . handleCase (charSing @'U', \Refl -> charSing @'u')
    . handleCase (charSing @'V', \Refl -> charSing @'v')
    . handleCase (charSing @'W', \Refl -> charSing @'w')
    . handleCase (charSing @'X', \Refl -> charSing @'x')
    . handleCase (charSing @'Y', \Refl -> charSing @'y')
    . handleCase (charSing @'Z', \Refl -> charSing @'z')
    $ unsafeCoerce sc
  where
    handleCase :: (SChar x, c :~: x -> SChar fx) -> SChar fx -> SChar fx
    handleCase = handleCaseWith sc

type family ToUpper (s :: Symbol) :: Symbol where
  ToUpper s = ToUpperUncons (UnconsSymbol s)

sToUpper :: SSymbol s -> SSymbol (ToUpper s)
sToUpper ss = sToUpperUncons (sUnconsSymbol ss)

type family ToUpperUncons (x :: Maybe (Char, Symbol)) :: Symbol where
  ToUpperUncons Nothing = ""
  ToUpperUncons (Just '(c, s)) = ConsSymbol (ToUpperChar c) (ToUpper s)

sToUpperUncons :: SMaybe (x :: Maybe (Char, Symbol)) -> SSymbol (ToUpperUncons x)
sToUpperUncons SNothing = symbolSing @""
sToUpperUncons (SJust (STuple2 sc ss)) =
  sConsSymbol (sToUpperChar sc) (sToUpper ss)

type family ToUpperChar (a :: Char) :: Char where
  ToUpperChar 'a' = 'A'
  ToUpperChar 'b' = 'B'
  ToUpperChar 'c' = 'C'
  ToUpperChar 'd' = 'D'
  ToUpperChar 'e' = 'E'
  ToUpperChar 'f' = 'F'
  ToUpperChar 'g' = 'G'
  ToUpperChar 'h' = 'H'
  ToUpperChar 'i' = 'I'
  ToUpperChar 'j' = 'J'
  ToUpperChar 'k' = 'K'
  ToUpperChar 'l' = 'L'
  ToUpperChar 'm' = 'M'
  ToUpperChar 'n' = 'N'
  ToUpperChar 'o' = 'O'
  ToUpperChar 'p' = 'P'
  ToUpperChar 'q' = 'Q'
  ToUpperChar 'r' = 'R'
  ToUpperChar 's' = 'S'
  ToUpperChar 't' = 'T'
  ToUpperChar 'u' = 'U'
  ToUpperChar 'v' = 'V'
  ToUpperChar 'w' = 'W'
  ToUpperChar 'x' = 'X'
  ToUpperChar 'y' = 'Y'
  ToUpperChar 'z' = 'Z'
  ToUpperChar x = x

sToUpperChar :: SChar c -> SChar (ToUpperChar c)
sToUpperChar = sToUpperChar_

sToUpperChar_ :: forall c. SChar c -> SChar (ToUpperChar c)
sToUpperChar_ sc =
  handleCase (charSing @'a', \Refl -> charSing @'A')
    . handleCase (charSing @'b', \Refl -> charSing @'B')
    . handleCase (charSing @'c', \Refl -> charSing @'C')
    . handleCase (charSing @'d', \Refl -> charSing @'D')
    . handleCase (charSing @'e', \Refl -> charSing @'E')
    . handleCase (charSing @'f', \Refl -> charSing @'F')
    . handleCase (charSing @'g', \Refl -> charSing @'G')
    . handleCase (charSing @'h', \Refl -> charSing @'H')
    . handleCase (charSing @'i', \Refl -> charSing @'I')
    . handleCase (charSing @'j', \Refl -> charSing @'J')
    . handleCase (charSing @'k', \Refl -> charSing @'K')
    . handleCase (charSing @'l', \Refl -> charSing @'L')
    . handleCase (charSing @'m', \Refl -> charSing @'M')
    . handleCase (charSing @'n', \Refl -> charSing @'N')
    . handleCase (charSing @'o', \Refl -> charSing @'O')
    . handleCase (charSing @'p', \Refl -> charSing @'P')
    . handleCase (charSing @'q', \Refl -> charSing @'Q')
    . handleCase (charSing @'r', \Refl -> charSing @'R')
    . handleCase (charSing @'s', \Refl -> charSing @'S')
    . handleCase (charSing @'t', \Refl -> charSing @'T')
    . handleCase (charSing @'u', \Refl -> charSing @'U')
    . handleCase (charSing @'v', \Refl -> charSing @'V')
    . handleCase (charSing @'w', \Refl -> charSing @'W')
    . handleCase (charSing @'x', \Refl -> charSing @'X')
    . handleCase (charSing @'y', \Refl -> charSing @'Y')
    . handleCase (charSing @'z', \Refl -> charSing @'Z')
    $ unsafeCoerce sc
  where
    handleCase :: (SChar x, c :~: x -> SChar fx) -> SChar fx -> SChar fx
    handleCase = handleCaseWith sc

handleCaseWith ::
  SChar c -> (SChar x, c :~: x -> SChar fx) -> SChar fx -> SChar fx
handleCaseWith sc (sx, f) g = maybe g f (testEquality sc sx)

type family IsPrefixOf (s :: Symbol) (t :: Symbol) :: Bool where
  IsPrefixOf s t = IsPrefixOfList (ToList s) (ToList t)

sIsPrefixOf :: SSymbol s -> SSymbol t -> SBool (IsPrefixOf s t)
sIsPrefixOf ss st = sIsPrefixOfList (sToList ss) (sToList st)

-- | Like 'IsPrefixOf', but case-insensitive
type family IsPrefixOfInsensitive (s :: Symbol) (t :: Symbol) :: Bool where
  IsPrefixOfInsensitive s t = IsPrefixOf (ToLower s) (ToLower t)

sIsPrefixOfInsensitive ::
  SSymbol s -> SSymbol t -> SBool (IsPrefixOfInsensitive s t)
sIsPrefixOfInsensitive ss st = sIsPrefixOf (sToLower ss) (sToLower st)

type family IsPrefixOfList (s :: [Char]) (t :: [Char]) :: Bool where
  IsPrefixOfList '[] '[] = True
  IsPrefixOfList '[] (y ': ys) = True
  IsPrefixOfList (x ': xs) (y ': ys) = (x == y) && IsPrefixOfList xs ys
  IsPrefixOfList (x ': xs) '[] = False

type family IsPrefixOfList1 (s :: [Char]) (t :: [Char]) :: Constraint where
  IsPrefixOfList1 '[] '[] = ()
  IsPrefixOfList1 '[] (y ': ys) = ()
  IsPrefixOfList1 (x ': xs) (x ': ys) = IsPrefixOfList1 xs ys

sIsPrefixOfList ::
  SList (s :: [Char]) -> SList (t :: [Char]) -> SBool (IsPrefixOfList s t)
sIsPrefixOfList SNil SNil = STrue
sIsPrefixOfList SNil (SCons _ _) = STrue
sIsPrefixOfList
  (SCons (sx :: SChar x) (sxs :: SList xs))
  (SCons (sy :: SChar y) (sys :: SList ys)) =
    let p1 :: SBool (x == y)
        p1 = unsafeCoerce (sx %== sy)

        p2 :: SBool (IsPrefixOfList xs ys)
        p2 = sIsPrefixOfList sxs sys
     in p1 %&& p2
sIsPrefixOfList (SCons _ _) SNil = SFalse

type family ToList (s :: Symbol) :: [Char] where
  ToList s = ToListUncons (UnconsSymbol s)

sToList :: SSymbol s -> SList (ToList s)
sToList = sToListUncons . sUnconsSymbol

type family ToListUncons (x :: (Maybe (Char, Symbol))) :: [Char] where
  ToListUncons 'Nothing = '[]
  ToListUncons ('Just '(c, s)) = c ': ToList s

sToListUncons :: SMaybe (x :: Maybe (Char, Symbol)) -> SList (ToListUncons x)
sToListUncons = \case
  SNothing -> SNil
  SJust (STuple2 sc ss) -> SCons sc (sToList ss)

-- | Singleton version of 'ConsSymbol'.
sConsSymbol :: SChar c -> SSymbol s -> SSymbol (ConsSymbol c s)
sConsSymbol sc ss = withSomeSSymbol (fromSChar sc : fromSSymbol ss) unsafeCoerce

-- | Singleton version of 'UnconsSymbol'.
sUnconsSymbol :: SSymbol s -> SMaybe (UnconsSymbol s)
sUnconsSymbol ss =
  case fromSSymbol ss of
    [] -> unsafeCoerce SNothing
    (c : s') ->
      withSomeSChar c $ \sc ->
        withSomeSSymbol s' $ \ss' ->
          unsafeCoerce (SJust (STuple2 sc ss'))
