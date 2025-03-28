-- SPDX-FileCopyrightText: Copyright Preetham Gujjula
-- SPDX-License-Identifier: BSD-3-Clause
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module VCard.Types.Textual.Private.AlphaNumDash
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

import Data.Bool.Singletons (SBool (SFalse, STrue), (%&&))
import Data.Constraint (Dict (..))
import Data.Kind (Constraint)
import Data.List.Singletons (SList (SCons, SNil))
import Data.Ord.Singletons ((%>), type (>))
import Data.Type.Bool (type (&&))
import GHC.TypeLits (Symbol)
import VCard.Char
  ( IsAlphaNumDashChar,
    IsAlphaNumDashLowerChar,
    IsAlphaNumDashUpperChar,
    sIsAlphaNumDashChar,
    sIsAlphaNumDashLowerChar,
    sIsAlphaNumDashUpperChar,
  )
import VCard.Util (Assert, NoInstance)
import VCard.Util.Natural (natSing)
import VCard.Util.Symbol
  ( Length,
    SSymbol,
    ToList,
    sLength,
    sToList,
  )

-- Writing AlphaNumDashSymbol/AlphaNumDashLowerSymbol/
-- AlphaNumDashUpperSymbol as type synonyms does not work on GHC 9.2. Once we
-- drop support for GHC 9.2 we can rewrite them as type synonyms.
type family AlphaNumDashSymbol (s :: Symbol) :: Constraint where
  AlphaNumDashSymbol s =
    Assert (IsAlphaNumDashSymbol s) (NoInstance "AlphaNumDashSymbol" s)

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
    Assert
      (IsAlphaNumDashLowerSymbol s)
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
    Assert
      (IsAlphaNumDashUpperSymbol s)
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
