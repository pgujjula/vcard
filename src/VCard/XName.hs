-- SPDX-FileCopyrightText: Copyright Preetham Gujjula
-- SPDX-License-Identifier: BSD-3-Clause
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module VCard.XName (XNameSymbol, XNameLowerSymbol, XNameUpperSymbol) where

import Data.Kind (Constraint)
import Data.Ord.Singletons (type (>))
import Data.Type.Bool (If, type (&&), type (||))
import Data.Type.Equality (type (==))
import GHC.TypeLits (ErrorMessage (ShowType, Text, (:<>:)), Symbol, TypeError)
import VCard.Symbol.Private (IsPrefixOf, IsPrefixOfInsensitive, Length, ToList)

type Valid :: Constraint
type Valid = ()

-- Writing XNameSymbol/XNameSymbolLower/XNameSymbolUpper as type synonyms does
-- not work on GHC 9.2. Once we drop support for GHC 9.2 we can rewrite them as
-- type synonyms.
type family XNameSymbol (s :: Symbol) :: Constraint where
  XNameSymbol s = If (IsXNameSymbol s) Valid (NoInstance "XNameSymbol" s)

type IsXNameSymbol s =
  IsPrefixOfInsensitive "x-" s && Length s > 2 && AllIsXChar (ToList s)

type family XNameLowerSymbol (s :: Symbol) :: Constraint where
  XNameLowerSymbol s =
    If (IsXNameLowerSymbol s) Valid (NoInstance "XNameLowerSymbol" s)

type IsXNameLowerSymbol s =
  IsPrefixOf "x-" s && Length s > 2 && AllIsXCharLower (ToList s)

type family XNameUpperSymbol (s :: Symbol) :: Constraint where
  XNameUpperSymbol s =
    If (IsXNameUpperSymbol s) Valid (NoInstance "XNameUpperSymbol" s)

type IsXNameUpperSymbol s =
  IsPrefixOf "X-" s && Length s > 2 && AllIsXCharUpper (ToList s)

type family AllIsXChar (xs :: [Char]) where
  AllIsXChar '[] = True
  AllIsXChar (x : xs) = IsXChar x && AllIsXChar xs

type family AllIsXCharLower (xs :: [Char]) where
  AllIsXCharLower '[] = True
  AllIsXCharLower (x : xs) = IsXCharLower x && AllIsXCharLower xs

type family AllIsXCharUpper (xs :: [Char]) where
  AllIsXCharUpper '[] = True
  AllIsXCharUpper (x : xs) = IsXCharUpper x && AllIsXCharUpper xs

type IsXChar c = IsAlpha c || IsDigit c || c == '-'

type IsXCharLower c = IsAlphaLower c || IsDigit c || c == '-'

type IsXCharUpper c = IsAlphaUpper c || IsDigit c || c == '-'

type IsAlpha c = IsAlphaLower c || IsAlphaUpper c

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
