-- SPDX-FileCopyrightText: Copyright Preetham Gujjula
-- SPDX-License-Identifier: BSD-3-Clause
module VCard.Util (crlf) where

import Data.Text (Text, pack)

crlf :: Text
crlf = pack "\r\n"
