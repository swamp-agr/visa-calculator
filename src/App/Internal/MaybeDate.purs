module App.Internal.MaybeDate where

import App.Internal.MaybeInt

import Prelude (class Show, show, ($), (<>), (<$>), (<*>))
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Foreign (toForeign)
import Data.Foreign.Class (class Encode)
import Data.Foreign.Generic (defaultOptions, genericEncode)
import Data.Newtype (class Newtype)
import Data.Maybe (Maybe(..), fromJust)
import Data.Date
import Data.Time.Duration
import Data.Semiring ((+))
import Data.Int (round, toNumber)
import Data.Number (fromString)

-- | MaybeDate | --
-- Simple newtype wrapper for Maybe Date

newtype MaybeDate = MaybeDate { unMaybeDate :: Maybe Date }

-- MaybeDate instances
instance showMaybeDate :: Show MaybeDate where
  show (MaybeDate val) =
    case val.unMaybeDate of
      Just x  -> show x
      Nothing -> ""

instance encodeMaybeDate :: Encode MaybeDate where
  encode (MaybeDate val) = toForeign $ (show val.unMaybeDate)

-- Helpers
emptyDate :: MaybeDate
emptyDate = MaybeDate { unMaybeDate : Nothing }

-- calculate Diff between two MaybeDate and wrap result in MaybeInt
calculateDiff :: MaybeDate -> MaybeDate -> MaybeInt
calculateDiff (MaybeDate {unMaybeDate:Nothing}) _ = emptyInt
calculateDiff _ (MaybeDate {unMaybeDate:Nothing}) = emptyInt
calculateDiff (MaybeDate { unMaybeDate: Just x }) (MaybeDate { unMaybeDate: Just y}) =
    MaybeInt { unMaybeInt: Just (extractDays z) }
    where z :: Days
          z = diff x y

extractDays (Days x) = round x