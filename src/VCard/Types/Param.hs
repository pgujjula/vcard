-- SPDX-FileCopyrightText: Copyright Preetham Gujjula
-- SPDX-License-Identifier: BSD-3-Clause

-- |
-- Module     : VCard.Types.Param
-- Copyright  : Copyright Preetham Gujjula
-- License    : BSD-3-Clause
-- Maintainer : Preetham Gujjula <libraries@mail.preetham.io>
-- Stability  : experimental
--
-- Types for the property parameters described in Section 5 of RFC 6350.
module VCard.Types.Param
  ( -- * Generic Param
    GenericParam (..),

    -- * Generic Param value

    -- ** Types
    ParamValue,
    ParamValueSymbol,
    testParamValueSymbol,
    SParamValue (..),
    SomeParamValue (..),

    -- ** Construction
    unParamValue,
    paramValueVal,
    someParamValueVal,

    -- ** Unquoting
    unquoteParamValue,
    UnquoteParamValueSymbol,
    sUnquoteSParamValue,

    -- * Section 5.1 @LANGUAGE@

    -- | /Reference:/ [/RFC 6350 Section 5.1/]
    --     (https://gist.github.com/pgujjula/af9bacba47664a58eea383a5ae44b10b#file-rfc6350-txt-L879)
    LanguageParam,

    -- * Section 5.2 @VALUE@

    -- | /Reference:/ [/RFC 6350 Section 5.2/]
    --     (https://gist.github.com/pgujjula/af9bacba47664a58eea383a5ae44b10b#file-rfc6350-txt-L896)
    ValueParam,
    ValueValue (..),

    -- * Section 5.3 @PREF@

    -- | /Reference:/ [/RFC 6350 Section 5.3/]
    --     (https://gist.github.com/pgujjula/af9bacba47664a58eea383a5ae44b10b#file-rfc6350-txt-L936)
    PrefParam,
    PrefValue (..),

    -- * Section 5.4 @ALTID@

    -- | /Reference:/ [/RFC 6350 Section 5.4/]
    --     (https://gist.github.com/pgujjula/af9bacba47664a58eea383a5ae44b10b#file-rfc6350-txt-L969)
    AltIDParam,
    SAltIDParam (..),
    SomeAltIDParam (..),
    altIDVal,
    someAltIDVal,

    -- * Section 5.5 @PID@

    -- | /Reference:/ [/RFC 6350 Section 5.5/]
    --     (https://gist.github.com/pgujjula/af9bacba47664a58eea383a5ae44b10b#file-rfc6350-txt-L1046)
    PIDParam,
    PIDValue (..),
    Digit,

    -- * Section 5.6 @TYPE@

    -- | /Reference:/ [/RFC 6350 Section 5.6/]
    --     (https://gist.github.com/pgujjula/af9bacba47664a58eea383a5ae44b10b#file-rfc6350-txt-L1046)
    TypeParam,
    TypeValue (..),

    -- * Section 5.7 @MEDIATYPE@

    -- | /Reference:/ [/RFC 6350 Section 5.7/]
    --     (https://gist.github.com/pgujjula/af9bacba47664a58eea383a5ae44b10b#file-rfc6350-txt-L1101)
    MediatypeParam,
    Mediatype (..),

    -- * Section 5.8 @CALSCALE@

    -- | /Reference:/ [/RFC 6350 Section 5.8/]
    --     (https://gist.github.com/pgujjula/af9bacba47664a58eea383a5ae44b10b#file-rfc6350-txt-L1121)
    CalscaleParam,
    CalscaleValue (..),

    -- * Section 5.9 @SORT-AS@

    -- | /Reference:/ [/RFC 6350 Section 5.9/]
    --     (https://gist.github.com/pgujjula/af9bacba47664a58eea383a5ae44b10b#file-rfc6350-txt-L1147)
    SortAsParam,

    -- * Section 5.10 @GEO@

    -- | /Reference:/ [/RFC 6350 Section 5.10/]
    --     (https://gist.github.com/pgujjula/af9bacba47664a58eea383a5ae44b10b#file-rfc6350-txt-L1214)
    GeoParam,
    GeoValue (..),

    -- * Section 5.11 @TZ@

    -- | /Reference:/ [/RFC 6350 Section 5.11/]
    --     (https://gist.github.com/pgujjula/af9bacba47664a58eea383a5ae44b10b#file-rfc6350-txt-L1224)
    TzParam,
    TzValue (..),

    -- * Any Param
    AnyParam (..),
  )
where

import VCard.Types.Param.AltID
  ( AltIDParam,
    SAltIDParam (..),
    SomeAltIDParam (..),
    altIDVal,
    someAltIDVal,
  )
import VCard.Types.Param.Any (AnyParam (..))
import VCard.Types.Param.Calscale (CalscaleParam, CalscaleValue (..))
import VCard.Types.Param.Generic (GenericParam (..))
import VCard.Types.Param.Geo (GeoParam, GeoValue (..))
import VCard.Types.Param.Language (LanguageParam)
import VCard.Types.Param.Mediatype (Mediatype (..), MediatypeParam)
import VCard.Types.Param.PID (Digit, PIDParam, PIDValue (..))
import VCard.Types.Param.ParamValue
  ( ParamValue,
    ParamValueSymbol,
    SParamValue (..),
    SomeParamValue (..),
    UnquoteParamValueSymbol,
    paramValueVal,
    sUnquoteSParamValue,
    someParamValueVal,
    testParamValueSymbol,
    unParamValue,
    unquoteParamValue,
  )
import VCard.Types.Param.Pref (PrefParam, PrefValue (..))
import VCard.Types.Param.SortAs (SortAsParam)
import VCard.Types.Param.Type (TypeParam, TypeValue (..))
import VCard.Types.Param.Tz (TzParam, TzValue (..))
import VCard.Types.Param.Value (ValueParam, ValueValue (..))
