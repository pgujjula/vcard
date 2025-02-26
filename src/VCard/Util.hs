-- SPDX-FileCopyrightText: Copyright Preetham Gujjula
-- SPDX-License-Identifier: BSD-3-Clause
module VCard.Util (crlf, dquote, intToText) where

import Data.Text (Text, pack)
import Data.Text.Lazy (toStrict)
import Data.Text.Lazy.Builder (toLazyText)
import Data.Text.Lazy.Builder.Int (decimal)

crlf :: Text
crlf = pack "\r\n"

dquote :: Text
dquote = pack "\x0022"

intToText :: (Integral a) => a -> Text
intToText = toStrict . toLazyText . decimal
