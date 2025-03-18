-- SPDX-FileCopyrightText: Copyright Preetham Gujjula
-- SPDX-License-Identifier: BSD-3-Clause

module Test.VCard.Char (tests) where

import Data.Singletons (fromSing)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, testCase, (@?=))
import VCard.Char
  ( crlf,
    dQuote,
    hTab,
    isAscii,
    isAsciiAlpha,
    isAsciiLower,
    isAsciiUpper,
    isDigit,
    isNonAscii,
    isQSafeChar,
    isSafeChar,
    isVChar,
    isValueChar,
    isWSP,
    sCRLF,
    sDQuote,
    sHTab,
    sIsAscii,
    sIsAsciiAlpha,
    sIsAsciiLower,
    sIsAsciiUpper,
    sIsDigit,
    sIsNonAscii,
    sIsQSafeChar,
    sIsSafeChar,
    sIsVChar,
    sIsValueChar,
    sIsWSP,
    sSpace,
    space,
  )
import VCard.Symbol.Private
  ( charSing,
    fromSChar,
    fromSSymbol,
  )

tests :: TestTree
tests =
  testGroup
    "Char"
    [ test_dQuote,
      test_crlf,
      test_isWSP,
      test_space,
      test_hTab,
      test_isAscii,
      test_isNonAscii,
      test_isVChar,
      test_isAsciiAlpha,
      test_isAsciiUpper,
      test_isAsciiLower,
      test_isDigit,
      test_isQSafeChar,
      test_isSafeChar,
      test_isValueChar
    ]

test_dQuote :: TestTree
test_dQuote =
  testCase "dQuote" $
    fromSChar sDQuote @?= dQuote

test_crlf :: TestTree
test_crlf =
  testCase "crlf" $
    fromSSymbol sCRLF @?= crlf

test_isWSP :: TestTree
test_isWSP =
  testCase "isWSP" $ do
    isWSP ' ' @?= True
    isWSP '\t' @?= True
    isWSP '\n' @?= False
    isWSP '\r' @?= False

    fromSing (sIsWSP (charSing @' ')) @?= True
    fromSing (sIsWSP (charSing @'\t')) @?= True
    fromSing (sIsWSP (charSing @'\n')) @?= False
    fromSing (sIsWSP (charSing @'\r')) @?= False

test_space :: TestTree
test_space =
  testCase "space" $ do
    fromSChar sSpace @?= space

test_hTab :: TestTree
test_hTab =
  testCase "hTab" $
    fromSChar sHTab @?= hTab

test_isAscii :: TestTree
test_isAscii =
  testCase "isAscii" $ do
    let assertIsAscii c = assertBool (show c <> " should be ASCII") (isAscii c)
    let assertNotIsAscii c =
          assertBool (show c <> " should not be ASCII") (not (isAscii c))
    assertIsAscii '\00'
    assertIsAscii '\40'
    assertIsAscii '\127'
    assertNotIsAscii '\128'
    assertNotIsAscii '\1000'
    assertNotIsAscii '\10000'

    let assertSIsAscii sc =
          assertBool (show sc <> " should be ASCII") (fromSing (sIsAscii sc))
    let assertNotSIsAscii sc =
          assertBool
            (show sc <> " should not be ASCII")
            (not (fromSing (sIsAscii sc)))
    assertSIsAscii (charSing @'\00')
    assertSIsAscii (charSing @'\40')
    assertSIsAscii (charSing @'\127')
    assertNotSIsAscii (charSing @'\128')
    assertNotSIsAscii (charSing @'\1000')
    assertNotSIsAscii (charSing @'\10000')

test_isNonAscii :: TestTree
test_isNonAscii =
  testCase "isNonAscii" $ do
    let assertIsNonAscii c =
          assertBool (show c <> " should not be ASCII") (isNonAscii c)
    let assertNotIsNonAscii c =
          assertBool (show c <> " should be ASCII") (not (isNonAscii c))
    assertIsNonAscii '\128'
    assertIsNonAscii '\1000'
    assertIsNonAscii '\10000'
    assertNotIsNonAscii '\00'
    assertNotIsNonAscii '\40'
    assertNotIsNonAscii '\127'

    let assertSIsNonAscii sc =
          assertBool
            (show sc <> " should not be ASCII")
            (fromSing (sIsNonAscii sc))
    let assertNotSIsNonAscii sc =
          assertBool
            (show sc <> " should be ASCII")
            (not (fromSing (sIsNonAscii sc)))
    assertSIsNonAscii (charSing @'\128')
    assertSIsNonAscii (charSing @'\1000')
    assertSIsNonAscii (charSing @'\10000')
    assertNotSIsNonAscii (charSing @'\00')
    assertNotSIsNonAscii (charSing @'\40')
    assertNotSIsNonAscii (charSing @'\127')

test_isVChar :: TestTree
test_isVChar =
  testCase "isVChar" $ do
    let assertIsVChar c =
          assertBool (show c <> " should be VChar") (isVChar c)
    let assertNotIsVChar c =
          assertBool (show c <> " should not be VChar") (not (isVChar c))
    assertNotIsVChar '\00'
    assertNotIsVChar '\10'
    assertNotIsVChar '\31'
    assertNotIsVChar '\32' -- space ' '
    assertIsVChar '\33' -- exclamation mark '!'
    assertIsVChar '\100'
    assertIsVChar '\126' -- tilde '~'
    assertNotIsVChar '\127' -- DEL
    assertNotIsVChar '\128' -- euro sign '€'
    assertNotIsVChar '\1000'
    assertNotIsVChar '\10000'

    let assertSIsVChar sc =
          assertBool
            (show sc <> " should be VChar")
            (fromSing (sIsVChar sc))
    let assertNotSIsVChar sc =
          assertBool
            (show sc <> " should not be VChar")
            (not (fromSing (sIsVChar sc)))
    assertNotSIsVChar (charSing @'\00')
    assertNotSIsVChar (charSing @'\10')
    assertNotSIsVChar (charSing @'\31')
    assertNotSIsVChar (charSing @'\32') -- space ' '
    assertSIsVChar (charSing @'\33') -- exclamation mark '!'
    assertSIsVChar (charSing @'\100')
    assertSIsVChar (charSing @'\126') -- tilde '~'
    assertNotSIsVChar (charSing @'\127') -- DEL
    assertNotSIsVChar (charSing @'\128') -- euro sign '€'
    assertNotSIsVChar (charSing @'\1000')
    assertNotSIsVChar (charSing @'\10000')

test_isAsciiAlpha :: TestTree
test_isAsciiAlpha =
  testCase "isAsciiAlpha" $ do
    let assertIsAsciiAlpha c =
          assertBool (show c <> " should be alphabetic ASCII") (isAsciiAlpha c)
    let assertNotIsAsciiAlpha c =
          assertBool
            (show c <> " should not be alphabetic ASCII")
            (not (isAsciiAlpha c))

    assertIsAsciiAlpha 'a'
    assertIsAsciiAlpha 'j'
    assertIsAsciiAlpha 'z'
    assertIsAsciiAlpha 'A'
    assertIsAsciiAlpha 'M'
    assertIsAsciiAlpha 'Z'

    assertNotIsAsciiAlpha '0'
    assertNotIsAsciiAlpha '5'
    assertNotIsAsciiAlpha '9'
    assertNotIsAsciiAlpha '.'
    assertNotIsAsciiAlpha '!'
    assertNotIsAsciiAlpha ' '
    assertNotIsAsciiAlpha 'é'

    let assertSIsAsciiAlpha sc =
          assertBool
            (show sc <> " should be alphabetic ASCII")
            (fromSing (sIsAsciiAlpha sc))
    let assertNotSIsAsciiAlpha sc =
          assertBool
            (show sc <> " should not be alphabetic ASCII")
            (not (fromSing (sIsAsciiAlpha sc)))

    assertSIsAsciiAlpha (charSing @'a')
    assertSIsAsciiAlpha (charSing @'j')
    assertSIsAsciiAlpha (charSing @'z')
    assertSIsAsciiAlpha (charSing @'A')
    assertSIsAsciiAlpha (charSing @'M')
    assertSIsAsciiAlpha (charSing @'Z')

    assertNotSIsAsciiAlpha (charSing @'0')
    assertNotSIsAsciiAlpha (charSing @'5')
    assertNotSIsAsciiAlpha (charSing @'9')
    assertNotSIsAsciiAlpha (charSing @'.')
    assertNotSIsAsciiAlpha (charSing @'!')
    assertNotSIsAsciiAlpha (charSing @' ')
    assertNotSIsAsciiAlpha (charSing @'é')

test_isAsciiUpper :: TestTree
test_isAsciiUpper =
  testCase "isAsciiUpper" $ do
    let assertIsAsciiUpper c =
          assertBool (show c <> " should be uppercase ASCII") (isAsciiUpper c)
    let assertNotIsAsciiUpper c =
          assertBool
            (show c <> " should not be uppercase ASCII")
            (not (isAsciiUpper c))

    assertIsAsciiUpper 'A'
    assertIsAsciiUpper 'M'
    assertIsAsciiUpper 'Z'

    assertNotIsAsciiUpper 'a'
    assertNotIsAsciiUpper 'j'
    assertNotIsAsciiUpper 'z'

    assertNotIsAsciiUpper '0'
    assertNotIsAsciiUpper '5'
    assertNotIsAsciiUpper '9'
    assertNotIsAsciiUpper '.'
    assertNotIsAsciiUpper '!'
    assertNotIsAsciiUpper ' '
    assertNotIsAsciiUpper 'é'

    let assertSIsAsciiUpper sc =
          assertBool
            (show sc <> " should be uppercase ASCII")
            (fromSing (sIsAsciiUpper sc))
    let assertNotSIsAsciiUpper sc =
          assertBool
            (show sc <> " should not be uppercase ASCII")
            (not (fromSing (sIsAsciiUpper sc)))

    assertSIsAsciiUpper (charSing @'A')
    assertSIsAsciiUpper (charSing @'M')
    assertSIsAsciiUpper (charSing @'Z')

    assertNotSIsAsciiUpper (charSing @'a')
    assertNotSIsAsciiUpper (charSing @'j')
    assertNotSIsAsciiUpper (charSing @'z')

    assertNotSIsAsciiUpper (charSing @'0')
    assertNotSIsAsciiUpper (charSing @'5')
    assertNotSIsAsciiUpper (charSing @'9')
    assertNotSIsAsciiUpper (charSing @'.')
    assertNotSIsAsciiUpper (charSing @'!')
    assertNotSIsAsciiUpper (charSing @' ')
    assertNotSIsAsciiUpper (charSing @'é')

test_isAsciiLower :: TestTree
test_isAsciiLower =
  testCase "isAsciiLower" $ do
    let assertIsAsciiLower c =
          assertBool (show c <> " should be lowercase ASCII") (isAsciiLower c)
    let assertNotIsAsciiLower c =
          assertBool
            (show c <> " should not be lowercase ASCII")
            (not (isAsciiLower c))

    assertIsAsciiLower 'a'
    assertIsAsciiLower 'j'
    assertIsAsciiLower 'z'

    assertNotIsAsciiLower 'A'
    assertNotIsAsciiLower 'M'
    assertNotIsAsciiLower 'Z'

    assertNotIsAsciiLower '0'
    assertNotIsAsciiLower '5'
    assertNotIsAsciiLower '9'
    assertNotIsAsciiLower '.'
    assertNotIsAsciiLower '!'
    assertNotIsAsciiLower ' '
    assertNotIsAsciiLower 'é'

    let assertSIsAsciiLower sc =
          assertBool
            (show sc <> " should be lowercase ASCII")
            (fromSing (sIsAsciiLower sc))
    let assertNotSIsAsciiLower sc =
          assertBool
            (show sc <> " should not be lowercase ASCII")
            (not (fromSing (sIsAsciiLower sc)))

    assertSIsAsciiLower (charSing @'a')
    assertSIsAsciiLower (charSing @'j')
    assertSIsAsciiLower (charSing @'z')

    assertNotSIsAsciiLower (charSing @'A')
    assertNotSIsAsciiLower (charSing @'M')
    assertNotSIsAsciiLower (charSing @'Z')

    assertNotSIsAsciiLower (charSing @'0')
    assertNotSIsAsciiLower (charSing @'5')
    assertNotSIsAsciiLower (charSing @'9')
    assertNotSIsAsciiLower (charSing @'.')
    assertNotSIsAsciiLower (charSing @'!')
    assertNotSIsAsciiLower (charSing @' ')
    assertNotSIsAsciiLower (charSing @'é')

test_isDigit :: TestTree
test_isDigit =
  testCase "isDigit" $ do
    let assertIsDigit c = assertBool (show c <> " should be digit") (isDigit c)
    let assertNotIsDigit c =
          assertBool (show c <> " should not be digit") (not (isDigit c))

    assertIsDigit '0'
    assertIsDigit '5'
    assertIsDigit '9'

    assertNotIsDigit 'a'
    assertNotIsDigit 'j'
    assertNotIsDigit 'z'

    assertNotIsDigit 'A'
    assertNotIsDigit 'M'
    assertNotIsDigit 'Z'

    assertNotIsDigit '.'
    assertNotIsDigit '!'
    assertNotIsDigit ' '
    assertNotIsDigit 'é'

    let assertSIsDigit sc =
          assertBool (show sc <> " should be digit") (fromSing (sIsDigit sc))
    let assertNotSIsDigit sc =
          assertBool
            (show sc <> " should not be digit")
            (not (fromSing (sIsDigit sc)))

    assertSIsDigit (charSing @'0')
    assertSIsDigit (charSing @'5')
    assertSIsDigit (charSing @'9')

    assertNotSIsDigit (charSing @'a')
    assertNotSIsDigit (charSing @'j')
    assertNotSIsDigit (charSing @'z')

    assertNotSIsDigit (charSing @'A')
    assertNotSIsDigit (charSing @'M')
    assertNotSIsDigit (charSing @'Z')

    assertNotSIsDigit (charSing @'.')
    assertNotSIsDigit (charSing @'!')
    assertNotSIsDigit (charSing @' ')
    assertNotSIsDigit (charSing @'é')

test_isQSafeChar :: TestTree
test_isQSafeChar =
  testCase "isQSafeChar" $ do
    let assertIsQSafeChar c =
          assertBool (show c <> " should be QSafe") (isQSafeChar c)
    let assertNotIsQSafeChar c =
          assertBool (show c <> " should not be QSafe") (not (isQSafeChar c))

    assertIsQSafeChar ' '
    assertIsQSafeChar '\t'
    assertNotIsQSafeChar '\n'
    assertNotIsQSafeChar '\r'

    assertIsQSafeChar '!' -- \x21
    assertNotIsQSafeChar '\"' -- \x22
    assertIsQSafeChar '\x23'
    assertIsQSafeChar '\x24'
    assertIsQSafeChar '\x25'
    assertIsQSafeChar '\x7c'
    assertIsQSafeChar '\x7d'
    assertIsQSafeChar '\x7e'

    assertNotIsQSafeChar '\x7f'

    assertIsQSafeChar '\x80'
    assertIsQSafeChar '\x81'
    assertIsQSafeChar '\x82'
    assertIsQSafeChar '\x100'
    assertIsQSafeChar '\x1000'

    let assertSIsQSafeChar sc =
          assertBool
            (show sc <> " should be QSafe")
            (fromSing (sIsQSafeChar sc))
    let assertNotSIsQSafeChar sc =
          assertBool
            (show sc <> " should not be QSafe")
            (not (fromSing (sIsQSafeChar sc)))

    assertSIsQSafeChar (charSing @' ')
    assertSIsQSafeChar (charSing @'\t')
    assertNotSIsQSafeChar (charSing @'\n')
    assertNotSIsQSafeChar (charSing @'\r')

    assertSIsQSafeChar (charSing @'!') -- \x21
    assertNotSIsQSafeChar (charSing @'\"') -- \x22
    assertSIsQSafeChar (charSing @'\x23')
    assertSIsQSafeChar (charSing @'\x24')
    assertSIsQSafeChar (charSing @'\x25')
    assertSIsQSafeChar (charSing @'\x7c')
    assertSIsQSafeChar (charSing @'\x7d')
    assertSIsQSafeChar (charSing @'\x7e')

    assertNotSIsQSafeChar (charSing @'\x7f')

    assertSIsQSafeChar (charSing @'\x80')
    assertSIsQSafeChar (charSing @'\x81')
    assertSIsQSafeChar (charSing @'\x82')
    assertSIsQSafeChar (charSing @'\x100')
    assertSIsQSafeChar (charSing @'\x1000')

test_isSafeChar :: TestTree
test_isSafeChar =
  testCase "isSafeChar" $ do
    let assertIsSafeChar c =
          assertBool (show c <> " should be SafeChar") (isSafeChar c)
    let assertNotIsSafeChar c =
          assertBool (show c <> " should not be SafeChar") (not (isSafeChar c))

    assertIsSafeChar ' '
    assertIsSafeChar '\t'
    assertNotIsSafeChar '\n'
    assertNotIsSafeChar '\r'

    assertIsSafeChar '!' -- \x21
    assertNotIsSafeChar '\"' -- \x22
    assertIsSafeChar '\x23'
    assertIsSafeChar '\x24'
    assertIsSafeChar '\x25'
    assertIsSafeChar '\x25'
    assertIsSafeChar '\x2a'
    assertIsSafeChar '\x2b'
    assertNotIsSafeChar '\x2c' -- ','
    assertIsSafeChar '\x2d'
    assertIsSafeChar '\x2e'
    assertIsSafeChar '\x37'
    assertIsSafeChar '\x38'
    assertIsSafeChar '\x39'
    assertNotIsSafeChar '\x3a' -- ':'
    assertNotIsSafeChar '\x3b' -- ';'
    assertIsSafeChar '\x3c'
    assertIsSafeChar '\x3d'
    assertIsSafeChar '\x3e'
    assertIsSafeChar '\x7c'
    assertIsSafeChar '\x7d'
    assertIsSafeChar '\x7e'

    assertNotIsSafeChar '\x7f'

    assertIsSafeChar '\x80'
    assertIsSafeChar '\x81'
    assertIsSafeChar '\x82'
    assertIsSafeChar '\x100'
    assertIsSafeChar '\x1000'

    let assertSIsSafeChar sc =
          assertBool
            (show sc <> " should be SafeChar")
            (fromSing (sIsSafeChar sc))
    let assertNotSIsSafeChar sc =
          assertBool
            (show sc <> " should not be SafeChar")
            (not (fromSing (sIsSafeChar sc)))

    assertSIsSafeChar (charSing @' ')
    assertSIsSafeChar (charSing @'\t')
    assertNotSIsSafeChar (charSing @'\n')
    assertNotSIsSafeChar (charSing @'\r')

    assertSIsSafeChar (charSing @'!') -- \x21
    assertNotSIsSafeChar (charSing @'\"') -- \x22
    assertSIsSafeChar (charSing @'\x23')
    assertSIsSafeChar (charSing @'\x24')
    assertSIsSafeChar (charSing @'\x25')
    assertSIsSafeChar (charSing @'\x25')
    assertSIsSafeChar (charSing @'\x2a')
    assertSIsSafeChar (charSing @'\x2b')
    assertNotSIsSafeChar (charSing @'\x2c') -- ','
    assertSIsSafeChar (charSing @'\x2d')
    assertSIsSafeChar (charSing @'\x2e')
    assertSIsSafeChar (charSing @'\x37')
    assertSIsSafeChar (charSing @'\x38')
    assertSIsSafeChar (charSing @'\x39')
    assertNotSIsSafeChar (charSing @'\x3a') -- ':'
    assertNotSIsSafeChar (charSing @'\x3b') -- ';'
    assertSIsSafeChar (charSing @'\x3c')
    assertSIsSafeChar (charSing @'\x3d')
    assertSIsSafeChar (charSing @'\x3e')
    assertSIsSafeChar (charSing @'\x7c')
    assertSIsSafeChar (charSing @'\x7d')
    assertSIsSafeChar (charSing @'\x7e')

    assertNotSIsSafeChar (charSing @'\x7f')

    assertSIsSafeChar (charSing @'\x80')
    assertSIsSafeChar (charSing @'\x81')
    assertSIsSafeChar (charSing @'\x82')
    assertSIsSafeChar (charSing @'\x100')
    assertSIsSafeChar (charSing @'\x1000')

test_isValueChar :: TestTree
test_isValueChar =
  testCase "isValueChar" $ do
    let assertIsValueChar c =
          assertBool (show c <> " should be ValueChar") (isValueChar c)
    let assertNotIsValueChar c =
          assertBool
            (show c <> " should not be ValueChar")
            (not (isValueChar c))

    assertIsValueChar ' '
    assertIsValueChar '\t'
    assertNotIsValueChar '\n'
    assertNotIsValueChar '\r'

    assertNotIsValueChar '\00'
    assertNotIsValueChar '\01'
    assertNotIsValueChar '\30'
    assertNotIsValueChar '\31'
    assertIsValueChar '\32' -- space ' '
    assertIsValueChar '\33' -- exclamation mark '!'
    assertIsValueChar '\100'
    assertIsValueChar '\126' -- tilde '~'
    assertNotIsValueChar '\127' -- DEL
    assertIsValueChar '\128' -- euro sign '€'
    assertIsValueChar '\129'
    assertIsValueChar '\1000'
    assertIsValueChar '\10000'

    let assertSIsValueChar sc =
          assertBool
            (show sc <> " should be ValueChar")
            (fromSing (sIsValueChar sc))
    let assertNotSIsValueChar sc =
          assertBool
            (show sc <> " should not be ValueChar")
            (not (fromSing (sIsValueChar sc)))

    assertSIsValueChar (charSing @' ')
    assertSIsValueChar (charSing @'\t')
    assertNotSIsValueChar (charSing @'\n')
    assertNotSIsValueChar (charSing @'\r')

    assertNotSIsValueChar (charSing @'\00')
    assertNotSIsValueChar (charSing @'\01')
    assertNotSIsValueChar (charSing @'\30')
    assertNotSIsValueChar (charSing @'\31')
    assertSIsValueChar (charSing @'\32') -- space ' '
    assertSIsValueChar (charSing @'\33') -- exclamation mark '!'
    assertSIsValueChar (charSing @'\100')
    assertSIsValueChar (charSing @'\126') -- tilde '~'
    assertNotSIsValueChar (charSing @'\127') -- DEL
    assertSIsValueChar (charSing @'\128') -- euro sign '€'
    assertSIsValueChar (charSing @'\129')
    assertSIsValueChar (charSing @'\1000')
    assertSIsValueChar (charSing @'\10000')
