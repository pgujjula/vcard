-- SPDX-FileCopyrightText: Copyright Preetham Gujjula
-- SPDX-License-Identifier: BSD-3-Clause
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module VCard.Symbol.Private.Case
  ( ToLower,
    sToLower,
    ToLowerChar,
    sToLowerChar,
    ToUpper,
    sToUpper,
    ToUpperChar,
    sToUpperChar,
  )
where

import Data.Maybe.Singletons (SMaybe (SJust, SNothing))
import Data.Tuple.Singletons (STuple2 (..))
import Data.Type.Equality ((:~:) (Refl))
import GHC.TypeLits (ConsSymbol, Symbol, UnconsSymbol)
import GHC.TypeLits.Singletons (sConsSymbol)
import Unsafe.Coerce (unsafeCoerce)
import VCard.Symbol.Private.Compat
  ( SChar,
    SSymbol,
    charSing,
    sUnconsSymbol,
    symbolSing,
    testSCharEquality,
  )

-- | Convert a 'Symbol' to lower case. Only affects ASCII characters, so @É@ is
--   not converted to @é@, for example.
type family ToLower (s :: Symbol) :: Symbol where
  ToLower s = ToLowerUncons (UnconsSymbol s)

-- | Singleton of 'ToLower'.
sToLower :: SSymbol s -> SSymbol (ToLower s)
sToLower ss = sToLowerUncons (sUnconsSymbol ss)

type family ToLowerUncons (mcs :: Maybe (Char, Symbol)) :: Symbol where
  ToLowerUncons Nothing = ""
  ToLowerUncons (Just '(c, s)) = ConsSymbol (ToLowerChar c) (ToLower s)

sToLowerUncons :: SMaybe mcs -> SSymbol (ToLowerUncons mcs)
sToLowerUncons = \case
  SNothing -> symbolSing @""
  SJust (STuple2 sc ss) -> sConsSymbol (sToLowerChar sc) (sToLower ss)

-- | Convert a 'Char' to lower case. Only affects ASCII characters, so @É@ is
--   not converted to @é@, for example.
type family ToLowerChar (c :: Char) :: Char where
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

-- | Singleton of 'ToLowerChar'.
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

-- | Convert a 'Symbol' to upper case. Only affects ASCII characters, so @é@ is
--   not converted to @É@, for example.
type family ToUpper (s :: Symbol) :: Symbol where
  ToUpper s = ToUpperUncons (UnconsSymbol s)

-- | Singleton of 'ToUpper'.
sToUpper :: SSymbol s -> SSymbol (ToUpper s)
sToUpper ss = sToUpperUncons (sUnconsSymbol ss)

type family ToUpperUncons (mcs :: Maybe (Char, Symbol)) :: Symbol where
  ToUpperUncons Nothing = ""
  ToUpperUncons (Just '(c, s)) = ConsSymbol (ToUpperChar c) (ToUpper s)

sToUpperUncons :: SMaybe mcs -> SSymbol (ToUpperUncons mcs)
sToUpperUncons = \case
  SNothing -> symbolSing @""
  SJust (STuple2 sc ss) -> sConsSymbol (sToUpperChar sc) (sToUpper ss)

-- | Convert a 'Char' to upper case. Only affects ASCII characters, so @é@ is
--   not converted to @É@, for example.
type family ToUpperChar (c :: Char) :: Char where
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
  ToUpperChar c = c

-- | Singleton of 'ToUpperChar'.
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
handleCaseWith sc (sx, f) g = maybe g f (testSCharEquality sc sx)
