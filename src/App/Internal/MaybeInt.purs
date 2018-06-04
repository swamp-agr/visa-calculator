module App.Internal.MaybeInt where

import Prelude (class Show, show, ($), (<>), (<$>), (<*>))
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Foreign (toForeign)
import Data.Foreign.Class (class Encode)
import Data.Foreign.Generic (defaultOptions, genericEncode)
import Data.Newtype (class Newtype)
import Data.Maybe (Maybe(..), fromJust)
import Data.Ord
import Data.Functor (map)
import Data.Int (fromString, round)

-- | MaybeInt | --
-- Simple newtype wrapper for Maybe Int

newtype MaybeInt = MaybeInt { unMaybeInt :: Maybe Int }

-- MaybeInt instances
instance showMaybeInt :: Show MaybeInt where
  show (MaybeInt val) =
    case val.unMaybeInt of
      Just x  -> show x
      Nothing -> ""

instance encodeMaybeInt :: Encode MaybeInt where
  encode (MaybeInt val) = toForeign $ (show val.unMaybeInt)

justZero :: MaybeInt
justZero = toInt 0

emptyInt :: MaybeInt
emptyInt = MaybeInt { unMaybeInt: Nothing }

toInt :: Int -> MaybeInt
toInt x = MaybeInt { unMaybeInt: Just x }

fromMaybeInt :: MaybeInt -> Int
fromMaybeInt (MaybeInt mi) =
  case mi.unMaybeInt of
    Nothing -> 0
    Just x  -> x

strToMaybeInt :: String -> MaybeInt
strToMaybeInt x = MaybeInt { unMaybeInt : fromString x }

nonNegativeOrEmpty :: MaybeInt -> Boolean
nonNegativeOrEmpty (MaybeInt {unMaybeInt: Nothing}) = true
nonNegativeOrEmpty (MaybeInt {unMaybeInt: Just x}) = if (x >= 0) then true else false

compareByInt :: MaybeInt -> MaybeInt -> Boolean
compareByInt (MaybeInt { unMaybeInt : Nothing }) _ = true
compareByInt _ (MaybeInt { unMaybeInt : Nothing }) = true
compareByInt (MaybeInt { unMaybeInt: Just x}) (MaybeInt { unMaybeInt: Just y}) = x < y
