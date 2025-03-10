-- SPDX-FileCopyrightText: Copyright Preetham Gujjula
-- SPDX-License-Identifier: BSD-3-Clause
{-# OPTIONS_GHC -Wno-orphans #-}

module VCard.Types.Value.URI
  ( URI (..),
  )
where

import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (pack, unpack)
import Network.URI (URI (..), parseURI)
import Text.Megaparsec (takeWhileP, try)
import VCard.Parse (HasParser (..), Parser)
import VCard.Serialize (HasSerializer (..), Serializer)

-- Characters that can appear in a URI. Taken from RFC 3986 Appendix A
uriCharSet :: Set Char
uriCharSet = Set.fromList uriChars
  where
    uriChars :: [Char]
    uriChars = scheme ++ [':'] ++ hierPart ++ ['?'] ++ query ++ ['#'] ++ fragment

    scheme :: [Char]
    scheme = alpha ++ digit ++ ['+', '-', '.']

    hierPart :: [Char]
    hierPart =
      ['/']
        ++ authority
        ++ pathAbempty
        ++ pathAbsolute
        ++ pathRootless
        ++ pathEmpty

    query :: [Char]
    query = pchar ++ ['/', '?']

    fragment :: [Char]
    fragment = pchar ++ ['/', '?']

    authority :: [Char]
    authority = userinfo ++ ['@'] ++ host ++ [':'] ++ port

    pathAbempty :: [Char]
    pathAbempty = '/' : segment

    pathAbsolute :: [Char]
    pathAbsolute = ['/'] ++ segmentNz ++ segment

    pathRootless :: [Char]
    pathRootless = ['/'] ++ segmentNz ++ segment

    pathEmpty :: [Char]
    pathEmpty = []

    alpha :: [Char]
    alpha = ['a' .. 'z'] ++ ['A' .. 'Z']

    digit :: [Char]
    digit = ['0' .. '9']

    pchar :: [Char]
    pchar = unreserved ++ pctEncoded ++ subDelims ++ [':']

    userinfo :: [Char]
    userinfo = unreserved ++ pctEncoded ++ subDelims ++ [':']

    host :: [Char]
    host = ipLiteral ++ ipv4Address ++ regName

    port :: [Char]
    port = digit

    segment :: [Char]
    segment = pchar

    segmentNz :: [Char]
    segmentNz = pchar

    unreserved :: [Char]
    unreserved = alpha ++ digit ++ ['-', '.', '_', '~']

    pctEncoded :: [Char]
    pctEncoded = '%' : hexdig

    subDelims :: [Char]
    subDelims = ['!', '$', '&', '\'', '(', ')', '*', '+', ',', ';', '=']

    ipLiteral :: [Char]
    ipLiteral = ['['] ++ ipv6Address ++ ipvFuture ++ [']']

    ipv4Address :: [Char]
    ipv4Address = digit ++ ['.']

    regName :: [Char]
    regName = unreserved ++ pctEncoded ++ subDelims

    hexdig :: [Char]
    hexdig = ['0' .. '9'] ++ ['a' .. 'f'] ++ ['A' .. 'F']

    ipv6Address :: [Char]
    ipv6Address = hexdig ++ [':'] ++ ipv4Address

    ipvFuture :: [Char]
    ipvFuture = ['v'] ++ hexdig ++ ['.'] ++ unreserved ++ subDelims ++ [':']

instance HasParser URI where
  parser :: Parser URI
  parser = try $ do
    chars <- takeWhileP (Just "uri") (`Set.member` uriCharSet)
    case parseURI (unpack chars) of
      Nothing -> fail "expected uri"
      Just uri -> pure uri

instance HasSerializer URI where
  serializer :: Serializer URI
  serializer = pack . show
