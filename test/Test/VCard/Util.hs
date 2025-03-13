-- SPDX-FileCopyrightText: Copyright Preetham Gujjula
-- SPDX-License-Identifier: BSD-3-Clause
{-# LANGUAGE StandaloneKindSignatures #-}

module Test.VCard.Util (Truth) where

import Data.Kind (Constraint)

type Truth :: Constraint
type Truth = ()
