-- SPDX-FileCopyrightText: Copyright Preetham Gujjula
-- SPDX-License-Identifier: BSD-3-Clause
{-# LANGUAGE CPP #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StandaloneDeriving #-}

-- |
-- Module     : VCard.Types.Property.ParamMap
-- Copyright  : Copyright Preetham Gujjula
-- License    : BSD-3-Clause
-- Maintainer : Preetham Gujjula <libraries@mail.preetham.io>
-- Stability  : experimental
--
-- 🚧 /Under construction./
--
-- Data structure for the parameters of a property.
module VCard.Types.Property.ParamMap
  ( -- * Types
    EmailParamMap (..),
    EmailParamTag (..),

    -- * Operations
    empty,
    insert,
    delete,
    lookup,

    -- * Example
    emailParams,
  )
where

import Data.Constraint (Dict (..))
import Data.Constraint.Extras (Has, argDict)
import Data.Dependent.Map (DMap)
import Data.Dependent.Map qualified as DMap
import Data.Dependent.Sum (DSum ((:=>)))
import Data.Finite (finite)
import Data.Functor.Identity (Identity (..))
import Data.GADT.Compare (GCompare (gcompare), GEq (geq), GOrdering (..))
import Data.GADT.Show (GShow, defaultGshowsPrec, gshowsPrec)
import Data.List.NonEmpty qualified as NonEmpty
import Data.Type.Equality ((:~:) (Refl))
import VCard.Types.Param
  ( GenericParam (..),
    PIDParam,
    PIDValue (..),
    PrefParam,
    PrefValue (..),
  )
import VCard.Types.Textual (CaseInsensitiveUpper (..))
import VCard.Util.Symbol (symbolSing)
import Prelude hiding (lookup)

--
-- Types
--

data EmailParamTag a where
  PIDParamTag :: EmailParamTag PIDParam
  PrefParamTag :: EmailParamTag PrefParam

-- We cannot derive Eq or Ord for EmailParamTag a when base < 4.19.0 because we
-- don't have Eq or Ord instances available for SSymbol s.
#if MIN_VERSION_base(4,19,0)
deriving instance Eq (EmailParamTag a)

deriving instance Ord (EmailParamTag a)
#else
instance Eq (EmailParamTag a) where
  _ == _ = True

instance Ord (EmailParamTag a) where
  compare _ _ = EQ
#endif

deriving instance Show (EmailParamTag a)

instance GShow EmailParamTag where gshowsPrec = defaultGshowsPrec

instance GEq EmailParamTag where
  geq PIDParamTag PIDParamTag = Just Refl
  geq PrefParamTag PrefParamTag = Just Refl
  geq _ _ = Nothing

instance GCompare EmailParamTag where
  gcompare PIDParamTag PIDParamTag = GEQ
  gcompare PIDParamTag PrefParamTag = GLT
  gcompare PrefParamTag PIDParamTag = GGT
  gcompare PrefParamTag PrefParamTag = GEQ

-- For the Show instance of DSum EmailParamTag Identity
instance (c PIDParam, c PrefParam) => Has c EmailParamTag where
  argDict :: EmailParamTag a -> Dict (c a)
  argDict = \case
    PIDParamTag -> Dict
    PrefParamTag -> Dict

-- | 'EmailParamMap' is a data structure for the parameters of an `EMAIL` property.
--
--   /Reference:/ [@EMAIL-param@](https://gist.github.com/pgujjula/af9bacba47664a58eea383a5ae44b10b#file-rfc6350-txt-L2003-L2004)
newtype EmailParamMap = EmailParamMap {unEmailParamMap :: DMap EmailParamTag Identity}

deriving instance Eq EmailParamMap

deriving instance Show EmailParamMap

--
-- Operations
--

-- | An empty 'EmailParamMap'.
empty :: EmailParamMap
empty = EmailParamMap DMap.empty

-- | Insert a key and value into an 'EmailParamMap'. If the key already exists
--   in the map, the previous value is overwritten.
insert :: EmailParamTag a -> a -> EmailParamMap -> EmailParamMap
insert k v = EmailParamMap . DMap.insert k (Identity v) . unEmailParamMap

-- | Delete a key and its value from the map. If the key does not exist, the
--   original map is returned.
delete :: EmailParamTag a -> EmailParamMap -> EmailParamMap
delete t = EmailParamMap . DMap.delete t . unEmailParamMap

-- | Lookup the value for a key, or 'Nothing' if the key does not exist.
lookup :: EmailParamTag a -> EmailParamMap -> Maybe a
lookup k = fmap runIdentity . DMap.lookup k . unEmailParamMap

--
-- Example
--

emailParams :: EmailParamMap
emailParams =
  EmailParamMap . DMap.fromList $
    [ PIDParamTag
        :=> Identity
          ( GenericParam
              { genericParamName = CaseInsensitiveUpper (symbolSing @"PID"),
                genericParamValue =
                  NonEmpty.singleton $
                    PIDValue
                      { pidValueWholePart = NonEmpty.singleton 3,
                        pidValueDecimalPart = Nothing
                      }
              }
          ),
      PrefParamTag
        :=> Identity
          ( GenericParam
              { genericParamName = CaseInsensitiveUpper (symbolSing @"PREF"),
                genericParamValue = PrefValue (finite 10)
              }
          )
    ]
