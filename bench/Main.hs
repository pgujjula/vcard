-- SPDX-FileCopyrightText: Copyright Preetham Gujjula
-- SPDX-License-Identifier: BSD-3-Clause

module Main (main) where

import Data.Text qualified as Text
import Test.Tasty.Bench
import VCard (parse, serialize)

main :: IO ()
main =
  defaultMain
    [ bench
        "parse and serialize"
        (nf (fmap serialize . parse) (Text.pack ""))
    ]
