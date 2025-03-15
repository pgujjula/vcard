-- SPDX-FileCopyrightText: Copyright Preetham Gujjula
-- SPDX-License-Identifier: BSD-3-Clause
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeFamilies #-}

-- | Model characters and character patterns.
module VCard.Char
  ( -- * Double-quote
    dQuote,
    DQuote,
    sDQuote,

    -- * CRLF
    crlf,
    CRLF,
    sCRLF,

    -- * Whitespace
    isWSP,
    IsWSP,
    sIsWSP,

    -- ** Space
    space,
    Space,
    sSpace,

    -- ** Horizontal tab
    hTab,
    HTab,
    sHTab,

    -- * ASCII

    -- | 'isAscii' here is re-exported from "Data.Char".
    isAscii,
    IsAscii,
    sIsAscii,

    -- * Non-ASCII
    isNonAscii,
    IsNonAscii,
    sIsNonAscii,

    -- * Visible ASCII
    isVChar,
    IsVChar,
    sIsVChar,

    -- * Alphabetic ASCII
    isAsciiAlpha,
    IsAsciiAlpha,
    sIsAsciiAlpha,

    -- ** Uppercase ASCII

    -- | 'isAsciiUpper' here is re-exported from "Data.Char".
    isAsciiUpper,
    IsAsciiUpper,
    sIsAsciiUpper,

    -- ** Lowercase ASCII

    -- | 'isAsciiLower' here is re-exported from "Data.Char".
    isAsciiLower,
    IsAsciiLower,
    sIsAsciiLower,

    -- * Digit

    -- | 'isDigit' here is re-exported from "Data.Char".
    isDigit,
    IsDigit,
    sIsDigit,

    -- * Quote-safe Char
    isQSafeChar,
    IsQSafeChar,
    sIsQSafeChar,

    -- * Safe Char
    isSafeChar,
    IsSafeChar,
    sIsSafeChar,

    -- * Value Char
    isValueChar,
    IsValueChar,
    sIsValueChar,
  )
where

import Data.Bool.Singletons
  ( Not,
    SBool,
    sNot,
    (%&&),
    (%||),
    type (&&),
    type (||),
  )
import Data.Char (isAscii, isAsciiLower, isAsciiUpper, isDigit)
import Data.Eq.Singletons ((%==), type (==))
import Data.Ord.Singletons ((%<), (%<=), type (<), type (<=))
import GHC.TypeLits (Symbol)
import VCard.Symbol.Private (SChar, SSymbol, charSing, symbolSing)

--
-- DQuote
--

-- | Double-quote character @'"'@.
dQuote :: Char
dQuote = '\"'

-- | Type-level double-quote.
type DQuote :: Char
type DQuote = '\"'

-- | Singleton for 'DQuote'.
sDQuote :: SChar DQuote
sDQuote = charSing

--
-- CRLF
--

-- | @"\\r\\n"@ sequence (technically not a character).
crlf :: [Char]
crlf = "\r\n"

-- | Type-level CRLF.
type CRLF :: Symbol
type CRLF = "\r\n"

-- | Singleton for 'CRLF'.
sCRLF :: SSymbol CRLF
sCRLF = symbolSing

--
-- Whitespace
--

-- | Character is space @' '@ or horizontal tab @\\t@.
isWSP :: Char -> Bool
isWSP c = c == space || c == hTab

-- | Type-level 'isWSP'.
type IsWSP :: Char -> Bool
type IsWSP c = c == Space || c == HTab

-- | Singleton for 'IsWSP'.
sIsWSP :: SChar c -> SBool (IsWSP c)
sIsWSP sc = sc %== sSpace %|| sc %== sHTab

-- | Space character @' '@.
space :: Char
space = ' '

-- | Type-level space character.
type Space :: Char
type Space = ' '

-- | Singleton for 'Space'.
sSpace :: SChar Space
sSpace = charSing

-- | Horizontal tab @'\\t'@.
hTab :: Char
hTab = '\t'

-- | Type-level horizontal tab.
type HTab :: Char
type HTab = '\t'

-- | Singleton for 'HTab'.
sHTab :: SChar HTab
sHTab = charSing @'\t'

--
-- ASCII
--

-- | Type-level 'isAscii'.
type IsAscii :: Char -> Bool
type IsAscii c = c < '\x80'

-- | Singleton for 'IsAscii'.
sIsAscii :: SChar c -> SBool (IsAscii c)
sIsAscii sc = sc %< charSing @'\x80'

--
-- Non-ASCII
--

-- | Selects non-ASCII characters.
isNonAscii :: Char -> Bool
isNonAscii = not . isAscii

-- | Type-level 'isNonAscii'.
type IsNonAscii :: Char -> Bool
type IsNonAscii c = Not (IsAscii c)

-- | Singleton for 'IsNonAscii'.
sIsNonAscii :: SChar c -> SBool (IsNonAscii c)
sIsNonAscii sc = sNot (sIsAscii sc)

--
-- Visible ASCII
--

-- | Character is in the range @['\\x21'..'\\x7e']@ corresponding to visible
--   (printable) ASCII characters.
isVChar :: Char -> Bool
isVChar c = '\x21' <= c && c <= '\x7e'

-- | Type-level 'isVChar'.
type IsVChar :: Char -> Bool
type IsVChar c = '\x21' <= c && c <= '\x7e'

-- | Singleton for 'sIsVChar'.
sIsVChar :: SChar c -> SBool (IsVChar c)
sIsVChar sc = charSing @'\x21' %<= sc %&& sc %<= charSing @'\x7e'

--
-- Alphabetic ASCII
--

-- | Selects alphabetic ASCII characters, i.e.,
--   @[\'a\'..\'z\'] ++ [\'A\'..\'Z\']@.
isAsciiAlpha :: Char -> Bool
isAsciiAlpha c = isAsciiUpper c || isAsciiLower c

-- | Type-level 'isAsciiAlpha'.
type IsAsciiAlpha :: Char -> Bool
type IsAsciiAlpha c = IsAsciiUpper c || IsAsciiLower c

-- | Singleton of 'sIsAsciiAlpha'.
sIsAsciiAlpha :: SChar c -> SBool (IsAsciiAlpha c)
sIsAsciiAlpha sc = sIsAsciiUpper sc %|| sIsAsciiLower sc

-- | Type-level 'isAsciiUpper'.
type IsAsciiUpper :: Char -> Bool
type IsAsciiUpper c = 'A' <= c && c <= 'Z'

-- | Singleton of 'IsAsciiUpper'.
sIsAsciiUpper :: SChar c -> SBool (IsAsciiUpper c)
sIsAsciiUpper sc = charSing @'A' %<= sc %&& sc %<= charSing @'Z'

-- | Type-level 'isAsciiLower'.
type IsAsciiLower :: Char -> Bool
type IsAsciiLower c = 'a' <= c && c <= 'z'

-- | Singleton of 'IsAsciiLower'.
sIsAsciiLower :: SChar c -> SBool (IsAsciiLower c)
sIsAsciiLower sc = charSing @'a' %<= sc %&& sc %<= charSing @'z'

--
-- Digit
--

-- | Type-level 'isDigit'.
type IsDigit :: Char -> Bool
type IsDigit c = '0' <= c && c <= '9'

-- | Singleton of 'IsDigit'.
sIsDigit :: SChar c -> SBool (IsDigit c)
sIsDigit sc = charSing @'0' %<= sc %&& sc %<= charSing @'9'

--
-- Quote-safe Char
--

-- | Selects characters matching QSAFE-CHAR in Section 3.3 of RFC 6350.
isQSafeChar :: Char -> Bool
isQSafeChar c =
  isWSP c
    || c == '!'
    || '\x23' <= c && c <= '\x7e'
    || isNonAscii c

-- | Type-level 'isQSafeChar'.
type IsQSafeChar :: Char -> Bool
type IsQSafeChar c =
  IsWSP c
    || c == '!'
    || '\x23' <= c && c <= '\x7e'
    || IsNonAscii c

-- | Singleton 'sIsQSafeChar'.
sIsQSafeChar :: SChar c -> SBool (IsQSafeChar c)
sIsQSafeChar sc =
  sIsWSP sc
    %|| sc %== charSing @'!'
    %|| charSing @'\x23' %<= sc %&& sc %<= charSing @'\x7e'
    %|| sIsNonAscii sc

--
-- Safe Char
--

-- | Selects characters matching SAFE-CHAR in Section 3.3 of RFC 6350.
isSafeChar :: Char -> Bool
isSafeChar c =
  isWSP c
    || c == '!'
    || '\x23' <= c && c <= '\x39'
    || '\x3c' <= c && c <= '\x7e'
    || isNonAscii c

-- | Type-level 'IsSafeChar'.
type IsSafeChar :: Char -> Bool
type IsSafeChar c =
  IsWSP c
    || c == '!'
    || '\x23' <= c && c <= '\x39'
    || '\x3c' <= c && c <= '\x7e'
    || IsNonAscii c

-- | Singleton of 'sIsSafeChar'.
sIsSafeChar :: SChar c -> SBool (IsSafeChar c)
sIsSafeChar sc =
  sIsWSP sc
    %|| sc %== charSing @'!'
    %|| charSing @'\x23' %<= sc %&& sc %<= charSing @'\x39'
    %|| charSing @'\x3c' %<= sc %&& sc %<= charSing @'\x7e'
    %|| sIsNonAscii sc

--
-- Value Char
--

-- | Selects characters matching VALUE-CHAR in Section 3.3 of RFC 6350.
isValueChar :: Char -> Bool
isValueChar c = isWSP c || isVChar c || isNonAscii c

-- | Type-level 'isValueChar'.
type IsValueChar :: Char -> Bool
type IsValueChar c = IsWSP c || IsVChar c || IsNonAscii c

-- | Singleton for 'IsValueChar'.
sIsValueChar :: SChar c -> SBool (IsValueChar c)
sIsValueChar sc = sIsWSP sc %|| sIsVChar sc %|| sIsNonAscii sc
