-- SPDX-FileCopyrightText: Copyright Preetham Gujjula
-- SPDX-License-Identifier: BSD-3-Clause
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module VCard.Types.Param.Language
  ( LanguageParam (..),
  )
where

import Control.Monad (void)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Type.Equality (testEquality, (:~:) (Refl))
import GHC.TypeLits (SSymbol, symbolSing, withSomeSSymbol)
import Text.Megaparsec.Char (char, string')
import VCard.Internal.Symbol
import VCard.Parse (HasParser (..), Parser)
import VCard.Types.Value (LanguageTag (..))

data LanguageParam where
  LanguageParam ::
    (ToUpper s ~ "LANGUAGE") =>
    { languageParamName :: SSymbol s,
      languageParamTag :: LanguageTag
    } ->
    LanguageParam

deriving instance Show LanguageParam

instance HasParser LanguageParam where
  parser :: Parser LanguageParam
  parser = do
    (t :: Text) <- string' "LANGUAGE"

    withSomeSSymbol (Text.unpack t) $ \(ss :: SSymbol s) ->
      case testEquality (sToUpper ss) (symbolSing @"LANGUAGE") of
        Just (Refl :: ToUpper s :~: "LANGUAGE") -> do
          void (char '-')
          tag <- parser @LanguageTag
          pure $
            LanguageParam
              { languageParamName = ss,
                languageParamTag = tag
              }
        Nothing -> fail "no language parse"
