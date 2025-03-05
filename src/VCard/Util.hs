-- SPDX-FileCopyrightText: Copyright Preetham Gujjula
-- SPDX-License-Identifier: BSD-3-Clause
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module VCard.Util
  ( crlf,
    dquote,
    intToText,
    sepByNonEmpty,
    intersperseCommaNE,
    Truth,
    NoInstance,
  )
where

import Control.Monad (MonadPlus)
import Data.Kind (Constraint)
import Data.List qualified as List
import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.List.NonEmpty qualified as NonEmpty
import Data.Text (Text, pack)
import Data.Text qualified as Text
import Data.Text.Lazy (toStrict)
import Data.Text.Lazy.Builder (toLazyText)
import Data.Text.Lazy.Builder.Int (decimal)
import GHC.TypeLits (ErrorMessage (ShowType, (:<>:)), Symbol, TypeError)
import GHC.TypeLits qualified as ErrorMessage (ErrorMessage (Text))
import Text.Megaparsec (many)
import VCard.Serialize (Serializer)

crlf :: Text
crlf = pack "\r\n"

dquote :: Text
dquote = pack "\x0022"

intToText :: (Integral a) => a -> Text
intToText = toStrict . toLazyText . decimal

sepByNonEmpty :: (MonadPlus m) => m a -> m sep -> m (NonEmpty a)
sepByNonEmpty p sep = do
  x <- p
  (x :|) <$> many (sep >> p)

intersperseCommaNE :: Serializer a -> Serializer (NonEmpty a)
intersperseCommaNE s xs =
  Text.concat (List.intersperse (Text.pack ",") (map s (NonEmpty.toList xs)))

type Truth :: Constraint
type Truth = ()

type family NoInstance (c :: Symbol) (s :: Symbol) :: Constraint where
  NoInstance c s =
    TypeError
      ( ErrorMessage.Text "No instance for ("
          :<>: ErrorMessage.Text c
          :<>: ErrorMessage.Text " "
          :<>: ShowType s
          :<>: ErrorMessage.Text ")"
      )
