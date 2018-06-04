module App.Internal.DateRange where

import App.Internal.MaybeDate
import App.Internal.MaybeInt

import Prelude (class Show, show, ($), (<>), (<$>), (<*>))
import Data.Date
import Data.Eq
import Data.Maybe (Maybe(..), fromJust)
import Data.Enum
import Data.Time.Duration
import Data.Int (round, toNumber)
import Data.Boolean
import Data.Ord
import Data.Ring ((-), sub, negate)
import Data.Semiring ((+))
import Data.Array
import Data.HeytingAlgebra ((||), not, (&&))
import Partial.Unsafe (unsafePartial)
import Data.Functor (map)
import Data.Function (flip)
import Data.Number
import Partial.Unsafe (unsafePartial)

import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Foreign (toForeign)
import Data.Foreign.Class (class Encode)
import Data.Foreign.Generic (defaultOptions, genericEncode)
import Data.Newtype (class Newtype)

-- | DateRange | --

newtype DateRange = DateRange
  { start :: MaybeDate
  , end :: MaybeDate
  , id :: Int
  , diff :: MaybeInt
  }

-- DateRange instances
derive instance genericDateRange :: Generic DateRange _
derive instance newtypeDateRange :: Newtype DateRange _

instance showDateRange :: Show DateRange where show = genericShow

instance encodeDateRange :: Encode DateRange where
  encode = genericEncode (defaultOptions { unwrapSingleConstructors = true })

-- DateRange Helpers
-- unit
emptyDateRange :: Int -> DateRange
emptyDateRange x = DateRange { start: emptyDate, end: emptyDate, id: x, diff: emptyInt }

-- today's 0 range
zeroDateRange :: Date -> Int -> DateRange
zeroDateRange d x = DateRange { start: md, end: md, id: x, diff: justZero }
  where md = MaybeDate { unMaybeDate: Just d }

-- merge overlaps
--   filtering out all ranges older than `dd` days in the past,
--   filtering out all ranges that are not dates, just to be sure
--   sorting by start date
--   making intersection
mergeOverlaps :: Date -> Int -> Array DateRange -> Array DateRange
mergeOverlaps today dd x = foldl overlap2 [] $ sortBy compareDateRanges $ filter justDateRange 
                   $ filter (outOfRange today dd) x

-- filtering nothing
justDateRange :: DateRange -> Boolean
justDateRange (DateRange { start: MaybeDate { unMaybeDate: Just _ }
                         , end  : MaybeDate { unMaybeDate: Just _ }
                         , id: _
                         , diff: _
                         }) = true
justDateRange _ = false

-- filtering ranges where end older than number of days ago `dd`
outOfRange :: Date -> Int -> DateRange -> Boolean
outOfRange today dd (DateRange { start: _
                               , end: MaybeDate { unMaybeDate: Just x }
                               , id: _
                               , diff: _
                               }) = (extractDays $ diff today x) - dd <= 0
outOfRange _ _ _ = false

compareDateRanges :: DateRange -> DateRange -> Ordering
compareDateRanges (DateRange { start: MaybeDate { unMaybeDate: Just x }
                             , end: _
                             , id: _
                             , diff: _
                             })
                  (DateRange { start: MaybeDate { unMaybeDate: Just y }
                             , end: _
                             , id: _
                             , diff: _
                             }) = compare x y
compareDateRanges _ _ = LT

overlap :: DateRange -> DateRange -> Boolean
overlap (DateRange { start: MaybeDate { unMaybeDate: Just x1 }
                   , end: MaybeDate { unMaybeDate: Just y1 }
                   , id: _
                   , diff: _
                   })
        (DateRange { start: MaybeDate { unMaybeDate: Just x2 }
                   , end: MaybeDate { unMaybeDate: Just y2 }
                   , id: _
                   , diff: _
                   }) = y1 <= x2 || x1 <= y2
overlap _ _ = false                             

combine :: DateRange -> DateRange -> DateRange
combine x@(DateRange { start: MaybeDate { unMaybeDate: Just x1 }
                   , end: MaybeDate { unMaybeDate: Just y1 }
                   , id: i1
                   , diff: _
                   })
        y@(DateRange { start: MaybeDate { unMaybeDate: Just x2 }
                   , end: MaybeDate { unMaybeDate: Just y2 }
                   , id: _
                   , diff: _
                   }) 
        = if overlap x y
            then DateRange { start: MaybeDate { unMaybeDate : Just x1 }
                           , end: MaybeDate { unMaybeDate : Just y2 }
                           , id: i1
                           , diff: MaybeInt { unMaybeInt: Just (extractDays $ diff x1 y2) }
                           }
            else y                
combine _ y = y

overlap2 :: Array DateRange -> DateRange -> Array DateRange
overlap2 [] x = [x]
overlap2 ys x = (combine (unsafePartial $ fromJust $ head ys) x)
                : (unsafePartial $ fromJust $ tail ys)
