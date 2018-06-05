module App.Internal.Period where

import App.Internal.DateRange (DateRange(..))
import App.Internal.MaybeDate (extractDays, MaybeDate(..))

import Prelude (class Show, show, ($), (<>), (<$>), (<*>))
import Data.Date
import Data.DateTime (DateTime(..), Time(..), date, adjust)
import Data.Eq
import Data.Maybe (Maybe(..), fromJust)
import Data.Enum
import Data.Either (Either(..))
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
import Data.Number.Format (toString)

import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Foreign (toForeign)
import Data.Foreign.Class (class Encode)
import Data.Foreign.Generic (defaultOptions, genericEncode)
import Data.Newtype (class Newtype)


-- | FuturePoint | --
-- Simple data structure for resolution about day in future while iterating by future ranges

newtype FuturePoint =
  FuturePoint
    { currentDate :: Date
    , currentNumberOfDays :: Int
    , lowerBound :: Int
    , upperBound :: Int
    }

-- FuturePoint instances
derive instance genericFuturePoint :: Generic FuturePoint _
derive instance newtypeFuturePoint :: Newtype FuturePoint _

instance showFuturePoint :: Show FuturePoint where show = genericShow

instance encodeFuturePoint :: Encode FuturePoint where
  encode (FuturePoint val) =
      toForeign $  "( currentDate = " <> (show val.currentDate)
                <> ", currentNumberOfDays = " <> (show val.currentNumberOfDays)
                <> ", lowerBound = " <> (show val.lowerBound)
                <> ", upperBOund = " <> (show val.upperBound) <> " )"


-- | ResultPoint | --
-- Simple data structure for result that should be rendered

newtype ResultPoint =
  ResultPoint
    { resultDate :: Date
    , daysToWait :: Int
    , direction :: Direction
    }

-- ResultPoint instances
derive instance genericResultPoint :: Generic ResultPoint _
derive instance newtypeResultPoint :: Newtype ResultPoint _

instance showResultPoint :: Show ResultPoint where
  show (ResultPoint { resultDate: rd, daysToWait: dtw, direction: dir}) = 
    case dir of
      None -> ""
      Now -> (showDate rd)
             <> " is a date where limit reached. You have 0 available days!"
      Below -> "Good! With given date, "
               <> (showDate rd)
               <> ", visa will expire in " <> (show $ negate dtw) <> " day(s)."
      Above -> "Something went wrong. With given date, "
               <> (showDate rd)
               <> ", visa expired " <> (show dtw) <> " day(s) ago."

instance encodeResultPoint :: Encode ResultPoint where
  encode (ResultPoint val) =
      toForeign $  "( currentDate = " <> (show val.resultDate)
                <> ", currentNumberOfDays = " <> (show val.daysToWait)
                <> ", direction = " <> (show val.direction) <> " )"

-- helpers
-- returns wrong result
emptyResultPoint :: ResultPoint
emptyResultPoint = ResultPoint { resultDate: yarrr, daysToWait: -99999, direction : None }

showDate :: Date -> String
showDate da = (show $ fromEnum y)
              <> "-" <> (justShowDatePart (fromEnum m))
              <> "-" <> (justShowDatePart (fromEnum d))
  where y = year da
        m = month da
        d = day da
        justShowDatePart x = if x < 10 then "0" <> (show x) else show x
  

-- | Direction | --
-- Types of direction

data Direction = Above | Below | Now | None

-- Direction instances
derive instance genericDirection :: Generic Direction _

instance showDirection :: Show Direction where show = genericShow

instance encodeDirection :: Encode Direction where
  encode = genericEncode (defaultOptions { unwrapSingleConstructors = true })

-- | Period | --
-- Simple data structure for Period of dates

newtype Period = Period { start :: Date, end :: Date }

-- Direction instances
derive instance genericPeriod :: Generic Period _
derive instance newtypePeriod :: Newtype Period _

instance eqDirection :: Eq Direction where
  eq Above Above = true
  eq Below Below = true
  eq Now Now = true
  eq None None = true
  eq x y = false

instance eqPeriod :: Eq Period where
  eq (Period x) (Period y) = (x.start == y.start) && (x.end == y.end)
instance showPeriod :: Show Period where show = genericShow

instance encodePeriod :: Encode Period where
  encode (Period x) = toForeign $ (show x.start) <> " - " <> (show x.end)

emptyPeriod :: Period
emptyPeriod = Period { start: yarrr, end: yarrr }

yarrr :: Date
yarrr = unsafePartial $ fromJust $ canonicalDate <$> toEnum 1900 <*> toEnum 1 <*> toEnum 1

oneDayPeriod :: Date -> Period
oneDayPeriod x = Period { start: x, end: x }

-- unboxing dates, removing ids, diffs
dateRangeToPeriod :: DateRange -> Period
dateRangeToPeriod (DateRange { start: MaybeDate { unMaybeDate: Just x }
                             , end :  MaybeDate { unMaybeDate: Just y }
                             , id: _
                             , diff: _
                             }) = Period { start: x, end: y }
dateRangeToPeriod _ = emptyPeriod

-- convert range to array of dates for further folding
periodToDates :: Period -> Array Date
periodToDates (Period { start: x, end: y }) = map (addDays x) (0 .. z)
  where z = (flip sub 1) $ extractDays $ diff y x

addDays :: Date -> Int -> Date
addDays d ix = date $ unsafePartial $ fromJust $ adjust (Days $ toNumber ix) (toDateTime d)

toDateTime :: Date -> DateTime
toDateTime d = DateTime d t
  where t = unsafePartial $ fromJust $ Time <$> toEnum 0 <*> toEnum 0 <*> toEnum 0 <*> toEnum 0

-- checking whether range is in the past or not, assuming that all empty dates filtered out
-- should be used after merged overlaps
rangeInPast :: Date -> Period -> Boolean
rangeInPast today (Period p) = (extractDays $ diff p.end today) <= 0

rangeInFuture :: Date -> Period -> Boolean
rangeInFuture today (Period p) = (extractDays $ diff p.start today) > 0

-- for given date, boundaries calculate and history of previous trips transform given date to "future date", i.e. we are in the future and we are trying to determine where limit will be reached
dateToFuturePoint :: Array Period -> Int -> Int -> Date -> FuturePoint
dateToFuturePoint xs lower upper y =
  FuturePoint
    { currentDate: y
    , currentNumberOfDays: cnod
    , lowerBound: lower
    , upperBound: upper
    }
  where ps = filter (not $ rangeInFuture y) xs
        startDay = addDays y (negate upper)
        ps' = mergePeriodStart startDay $ mergePeriodEnd y ps
        ps'' = filter (not $ rangeInPast startDay) ps'
        alldays = concatMap periodToDates ps''
        cnod = (length alldays) - lower

mergePeriodEnd :: Date -> Array Period -> Array Period
mergePeriodEnd x [] = []
mergePeriodEnd dat range =
  if inPeriod dat lst
     then snoc range' $ mergeMinEnd dat lst
     -- else snoc range' $ oneDayPeriod dat
     else range
  where lst = unsafePartial $ fromJust $ last range
        range' = safeInit range

safeInit :: forall a. Array a -> Array a
safeInit [] = []
safeInit x = unsafePartial (fromJust (init x))
       
mergePeriodStart :: Date -> Array Period -> Array Period
mergePeriodStart x [] = []
mergePeriodStart x ys =
  if inPeriod x y'
     then cons (mergeMaxStart x y') ys'
     -- else cons (oneDayPeriod x) ys
     else ys
  where y' = unsafePartial $ fromJust $ head ys
        ys' = safetail ys
        safetail [] = []
        safetail l = unsafePartial $ fromJust $ tail l

mergeMaxStart :: Date -> Period -> Period
mergeMaxStart d (Period p) =
  if p.start < d
    then Period { start: d, end: p.end }
    else Period p

mergeMinEnd :: Date -> Period -> Period
mergeMinEnd d (Period p) =
  if d < p.end
    then Period { start: p.start, end: d }
    else Period p

inPeriod :: Date -> Period -> Boolean
inPeriod d (Period { start: x, end: y }) = x <= d && d <= y

futureToResultPoint :: FuturePoint -> ResultPoint
futureToResultPoint (FuturePoint f) =
  ResultPoint { resultDate : f.currentDate, daysToWait: f.currentNumberOfDays, direction: decide f.currentNumberOfDays }
  where decide 0 = Now
        decide x = if x < 0 then Below else Above

limitReached :: ResultPoint -> Boolean
limitReached (ResultPoint r) = r.direction == Now

algorithm :: Date -> Int -> Int -> Array Period -> ResultPoint
algorithm _ _ _ [] = emptyResultPoint
algorithm today lower upper xs =
  if null xss
    then emptyResultPoint
    else 
      if null fs
        then x2rp
        else if null rps'
             then if null rps
                  then emptyResultPoint
                  else unsafePartial $ fromJust $ head rps
             else unsafePartial $ fromJust $ head rps'
  where xss = filter (notEq z2) xs
        fs = filter (not $ rangeInPast today) xss
        fds = concatMap periodToDates fs
        fps = map (dateToFuturePoint xss lower upper) fds
        rps = map futureToResultPoint fps
        rps' = dropWhile (not $ limitReached) rps
        z2 = emptyPeriod
        x2rp = futureToResultPoint $ dateToFuturePoint xss lower upper today

justShowDatePartE day1 def =
  case day1 of
      Right x -> toString x
      Left _ -> def

printPeriod :: Period -> String
printPeriod (Period p) = "start: " <> (show p.start) <> "\tend: " <> (show p.end)