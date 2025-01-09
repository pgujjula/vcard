-- SPDX-FileCopyrightText: Copyright Preetham Gujjula
-- SPDX-License-Identifier: BSD-3-Clause

-- |
-- Module     : VCard.Types.Value
-- Copyright  : Copyright Preetham Gujjula
-- License    : BSD-3-Clause
-- Maintainer : Preetham Gujjula <libraries@mail.preetham.io>
-- Stability  : experimental
--
-- Types for the property value data types described in Section 4 of RFC 6350.
module VCard.Types.Value
  ( -- * Section 4.1 @TEXT@
    Text,
    TextList,

    -- * Section 4.2 @URI@

    -- | The 'URI' type here is re-exported from "Network.URI". This works well
    -- because Network.URI implements RFC 3986, which is also the basis of the
    -- URI type in the vCard spec.
    URI (..),
    List,

    -- * Section 4.3.1 @DATE@

    -- ** 'Year'
    Year (..),
    HasYear (..),

    -- ** 'Month'
    Month (..),
    HasMonth (..),

    -- ** 'Day'
    Day (..),

    -- ** 'YearMonth'
    YearMonth (..),
  )
where

import VCard.Types.Value.Date
  ( Day (..),
    HasMonth (..),
    HasYear (..),
    Month (..),
    Year (..),
    YearMonth (..),
  )
import VCard.Types.Value.List (List (..))
import VCard.Types.Value.Text (Text (..), TextList)
import VCard.Types.Value.URI (URI (..))
