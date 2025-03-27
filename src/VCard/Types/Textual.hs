-- SPDX-FileCopyrightText: Copyright Preetham Gujjula
-- SPDX-License-Identifier: BSD-3-Clause

module VCard.Types.Textual
  ( -- * @CaseInsensitive@
    CaseInsensitiveLower (..),
    CaseInsensitiveUpper (..),

    -- * @XName@

    -- ** @XNameSymbol@
    XNameSymbol,
    IsXNameSymbol,
    sIsXNameSymbol,
    testXNameSymbol,

    -- ** @XName@ data types
    XName (unXName),
    SXName (..),
    SomeXName (..),
    xNameVal,
    someXNameVal,

    -- ** @XNameLower@
    XNameLowerSymbol,
    IsXNameLowerSymbol,
    sIsXNameLowerSymbol,
    testXNameLowerSymbol,

    -- ** @XNameUpper@
    XNameUpperSymbol,
    IsXNameUpperSymbol,
    sIsXNameUpperSymbol,
    testXNameUpperSymbol,

    -- * @AlphaNumDash@
    AlphaNumDashSymbol,
    testAlphaNumDashSymbol,
    IsAlphaNumDashSymbol,
    sIsAlphaNumDashSymbol,
    --
    AlphaNumDashLowerSymbol,
    testAlphaNumDashLowerSymbol,
    IsAlphaNumDashLowerSymbol,
    sIsAlphaNumDashLowerSymbol,
    --
    AlphaNumDashUpperSymbol,
    testAlphaNumDashUpperSymbol,
    IsAlphaNumDashUpperSymbol,
    sIsAlphaNumDashUpperSymbol,
  )
where

import VCard.Types.Textual.Private.AlphaNumDash
  ( AlphaNumDashLowerSymbol,
    AlphaNumDashSymbol,
    AlphaNumDashUpperSymbol,
    IsAlphaNumDashLowerSymbol,
    IsAlphaNumDashSymbol,
    IsAlphaNumDashUpperSymbol,
    sIsAlphaNumDashLowerSymbol,
    sIsAlphaNumDashSymbol,
    sIsAlphaNumDashUpperSymbol,
    testAlphaNumDashLowerSymbol,
    testAlphaNumDashSymbol,
    testAlphaNumDashUpperSymbol,
  )
import VCard.Types.Textual.Private.CaseInsensitive
  ( CaseInsensitiveLower (..),
    CaseInsensitiveUpper (..),
  )
import VCard.Types.Textual.Private.XName
  ( IsXNameLowerSymbol,
    IsXNameSymbol,
    IsXNameUpperSymbol,
    SXName (..),
    SomeXName (..),
    XName (unXName),
    XNameLowerSymbol,
    XNameSymbol,
    XNameUpperSymbol,
    sIsXNameLowerSymbol,
    sIsXNameSymbol,
    sIsXNameUpperSymbol,
    someXNameVal,
    testXNameLowerSymbol,
    testXNameSymbol,
    testXNameUpperSymbol,
    xNameVal,
  )
