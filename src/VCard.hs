-- SPDX-FileCopyrightText: Copyright Preetham Gujjula
-- SPDX-License-Identifier: BSD-3-Clause

module VCard (serialize, parse) where

import Data.Text (Text)
import Data.Text qualified as Text
import VCard.Types (VCard)

serialize :: VCard -> Text
serialize _ = Text.pack ""

parse :: Text -> Maybe VCard
parse _ = Nothing
