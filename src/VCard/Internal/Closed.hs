-- SPDX-FileCopyrightText: Copyright Preetham Gujjula
-- SPDX-License-Identifier: BSD-3-Clause
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module VCard.Internal.Closed
  ( Closed,
  )
where

import Data.Proxy (Proxy (..))
import GHC.TypeLits (ErrorMessage (..), TypeError)

-- The following code is adapted from the `closed-classes` project, available on
-- Hackage:
--
--   https://hackage.haskell.org/package/closed-classes-0.1
--
-- It is licensed under the following terms:
--
-- SPDX-SnippetBegin
-- SPDX-SnippetCopyrightText: Copyright (c) 2021, Anthony Vandikas
-- SPDX-License-Identifier: BSD-3-Clause

class Closed (a :: k) where
  default privateClosed :: (Closed (Closed @k)) => Proxy a
  privateClosed :: Proxy a
  privateClosed = Proxy

instance (TypeError ('Text "User defined " ':<>: 'ShowType a ':<>: 'Text " instances are not allowed.")) => Closed a where
  privateClosed = Proxy

-- SPDX-SnippetEnd
