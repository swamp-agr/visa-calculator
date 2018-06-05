module App.Internal.DateRange where

import App.Internal.MaybeDate
import App.Internal.MaybeInt

import Prelude (class Show, show, ($), (<>), (<$>), (<*>), bind, discard, pure)
import Control.MonadZero (guard)
import Control.Semigroupoid ((<<<))
import Data.Date
import Data.Eq
import Data.Maybe (Maybe(..), fromJust)
import Data.Enum
import Data.Time.Duration
import Data.Int (round, toNumber)
import Data.Boolean
import Data.Ord
import Data.Tuple
import Data.Ring ((-), sub, negate)
import Data.Semiring ((+))
import Data.Array
import Data.HeytingAlgebra ((||), not, (&&))
import Partial.Unsafe (unsafePartial)
import Data.Foldable (and, or)
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

instance eqDateRange :: Eq DateRange where
  eq (DateRange { start: MaybeDate { unMaybeDate: Just x1}
                , end  : MaybeDate { unMaybeDate: Just x2}
                , id   : _
                , diff : _
                })
     (DateRange { start: MaybeDate { unMaybeDate: Just y1}
                , end  : MaybeDate { unMaybeDate: Just y2}
                , id   : _
                , diff : _
                }) = x1 == y1 && x2 == y2
  eq _ _ = false

-- DateRange Helpers
-- unit
emptyDateRange :: Int -> DateRange
emptyDateRange x = DateRange { start: emptyDate, end: emptyDate, id: x, diff: emptyInt }

-- builder
makeDateRange :: MaybeDate -> MaybeDate -> Int -> DateRange
makeDateRange sd ed id' = DateRange { start: sd, end: ed, id: id', diff: emptyInt }

-- today's 0 range
zeroDateRange :: Date -> Int -> DateRange
zeroDateRange d x = DateRange { start: md, end: md, id: x, diff: justZero }
  where md = MaybeDate { unMaybeDate: Just d }

-- merge overlaps
--   filtering out all ranges older than `dd` days in the past,
--   filtering out all ranges that are not dates, just to be sure
--   sorting by start date
--   making intersection
mergeOverlaps :: Array DateRange -> Array DateRange
mergeOverlaps x = nub 
  -- $ foldl overlap2 []
  -- $ scanl overlap2 []
  $ overlap3 [] x2
  where x2 = sortBy compareDateRanges $ filter justDateRange x

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

-- overlap between 1st and 2nd daterange. we consider first as a key to make lookup into the list
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
                   }) = not ((y2 < x1) || (y1 < x2))
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
            then DateRange { start: MaybeDate { unMaybeDate : Just (min x1 x2) }
                           , end: MaybeDate { unMaybeDate : Just (max y1 y2) }
                           , id: i1
                           , diff: MaybeInt { unMaybeInt: Just (extractDays $ diff (min x1 x2) (max y1 y2)) }
                           }
            else y
combine _ y = y

overlap2 :: Array DateRange -> DateRange -> Array DateRange
overlap2 [] x = [x]
overlap2 ys x = map (flip combine x) ys

overlap3 :: Array DateRange -> Array DateRange -> Array DateRange
overlap3 x [] = x
overlap3 [] ys = overlap3 (take 1 ys) (drop 1 ys)
overlap3 xs ys =
  if null xs
  then overlap3 [] ys
  else if null ys
       then xs
       else if flag
            then overlap3 ((combine y (snd $ unsafePartial $ fromJust $ head zipFlags)):(drop 1 xs)) (drop 1 ys)
            else overlap3 (y:xs) (drop 1 ys)
  where y = unsafePartial $ fromJust $ head ys
        x = unsafePartial $ fromJust $ head xs
        flags = map (overlap x) ys
        flag = or flags
        zipFlags = filter (notEq $ Tuple true y) $ filter ((notEq true) <<< fst) $ zip flags ys        
        z = drop 1 ys

overlap4 :: Array DateRange -> Array (Tuple DateRange DateRange)
overlap4 xs = do
  i <- xs
  j <- xs
  guard $ overlap i j == true && i /= j
  pure $ Tuple i (combine i j)

merge1 :: DateRange -> Array (Tuple DateRange DateRange) -> DateRange
merge1 x ys =
    if null z
    then x
    else snd $ unsafePartial $ fromJust $ head z
  where z = filter (eq x <<< fst) $ ys

overlap5 :: Array DateRange -> Array DateRange
overlap5 xs = nub $ map (flip merge1 y) xs
  where y = overlap4 xs

overlap6 :: Array DateRange -> Array DateRange
overlap6 xs = go (l - 1) x2
  where l = length xs
        x2 = sortBy compareDateRanges $ filter justDateRange xs

go :: Int -> Array DateRange -> Array DateRange
go 0 xs = overlap5 xs
go x ys = nub $ go (x-1) z
 where z = overlap5 ys

  