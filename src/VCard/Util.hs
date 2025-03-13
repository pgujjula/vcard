-- SPDX-FileCopyrightText: Copyright Preetham Gujjula
-- SPDX-License-Identifier: BSD-3-Clause
{-# LANGUAGE CPP #-}
#if !MIN_VERSION_base(4,17,0)
{-# LANGUAGE StandaloneKindSignatures #-}
#endif
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module VCard.Util
  ( crlf,
    dquote,
    intToText,
    sepByNonEmpty,
    intersperseCommaNE,
    NoInstance,
    Assert,
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
#if MIN_VERSION_base(4,17,0)
import GHC.TypeError (Assert)
#endif
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

type family NoInstance (c :: Symbol) (s :: Symbol) :: Constraint where
  NoInstance c s =
    TypeError
      ( ErrorMessage.Text "No instance for ("
          :<>: ErrorMessage.Text c
          :<>: ErrorMessage.Text " "
          :<>: ShowType s
          :<>: ErrorMessage.Text ")"
      )

#if !MIN_VERSION_base(4,17,0)
-- | When GHC < 9.4, `Assert` is not available from `base`, so we define
--   our own.
type Assert :: Bool -> Constraint -> Constraint
type family Assert check errMsg where
  Assert 'True _      = ()
  Assert _     errMsg = errMsg
#endif
