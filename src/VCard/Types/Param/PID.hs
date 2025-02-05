-- SPDX-FileCopyrightText: Copyright Preetham Gujjula
-- SPDX-License-Identifier: BSD-3-Clause

module VCard.Types.Param.PID
  ( PIDParam (..),
    PIDValue (..),
  )
where

import Data.List.NonEmpty (NonEmpty)
import Numeric.Natural (Natural)

newtype PIDParam = PIDParam {unPIDParam :: NonEmpty PIDValue}
  deriving (Eq, Show, Ord)

data PIDValue = PIDValue
  { pidValueWholePart :: Natural, -- Maybe Word?
    pidValueDecimalPart :: Natural
  }
  deriving (Eq, Show, Ord)
