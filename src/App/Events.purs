module App.Events where

import Prelude ((<<<), (/=), (==), bind, discard)
import App.Routes (Route)
import App.State
import App.Internal.UserRange
import App.Internal.ErrorMsg
import App.Internal.DateWidget
import App.Internal.Period
import App.Internal.MaybeDate
import App.Internal.MaybeInt
import App.Internal.DateRange

import Data.Function (($))
import Network.HTTP.Affjax (AJAX)
import Pux (EffModel, noEffects)
import Data.Monoid ((<>))
import Pux.DOM.Events (DOMEvent, targetValue)
import Data.DateTime (Date, DateTime(..), Time(..), date)
import Data.Date (diff)
import Control.Monad.Eff.Now (NOW, nowDateTime)
import Control.Applicative (pure)
import Control.Apply ((<$>),(<*>))
import Data.Maybe (Maybe(..), fromJust)
import Control.Comonad (extract)
import Data.Array ((:), filter, head, length, concatMap, dropWhile, sortBy, (!!), singleton)
import Data.Ring ((-), sub, negate)
import Data.Semiring ((+))
import Data.Semigroup (append)
import Data.JSDate (LOCALE, fromDateTime, getUTCFullYear, getUTCMonth, parse, toDate, getDate, JSDate(..))
import Control.Monad.Aff (liftEff')
import Data.Foldable
import Data.Either (Either(..))
import Data.Functor (map)
import Data.Enum (toEnum)
import Partial.Unsafe (unsafePartial)
import Data.Show (show)
import Data.String (length) as String
import Data.String.Regex (Regex, regex, test)
import Data.String.Regex.Flags (noFlags)
import Data.Number.Format (toString)
import Data.Int (toNumber, round)
import Control.Monad.Eff.Console
import Data.Ord
import Data.Eq
import Data.Time.Duration
import Data.HeytingAlgebra
import Data.Function (const)

data Event = PageView Route
           -- processing user input, triggered by TryParse
           | ChangeRange Attr Int Date String
           | ChangeRangeError Attr Validation Int String
           -- event triggered by AddPeriod
           | GetNow
           -- adding period by date & removing period by its id
           | AddPeriod DateTime String | RemovePeriod Int
           -- processing user input, raw
           | TryParse Attr Int DOMEvent
           -- processing user input for lower and upper bounds
           | ProvideLower DOMEvent
           | ProvideUpper DOMEvent
           -- Final event to produce the main result if everything is correct
           | CalculateDays

type AppEffects fx = (ajax :: AJAX, now :: NOW, locale :: LOCALE, console :: CONSOLE | fx)

foldp :: ∀ fx. Event -> State -> EffModel State Event (AppEffects fx)
foldp (PageView route) (State st) = noEffects $
  State st { route = route, loaded = true }

foldp (TryParse attr pid ev) (State st) =
  { state: State st
  , effects:
    [ do let tv = targetValue ev
         regTest <- pure $ test dateRegex tv
         if regTest
           then do
             val <- liftEff' $ parse tv
             case val of
               Left err -> pure Nothing
               Right v -> do
                 case (toDate v) of
                   Just d -> pure $ Just $ ChangeRange attr pid d tv
                   Nothing -> pure Nothing
           else pure $ Just $ ChangeRangeError attr invalid pid tv
    ]
  }
  where invalid = whatError attr

foldp (ProvideLower ev) (State st) = 
    { state: State st { visa = setVisaMsg errMessage (vi st.visa lb) }
    , effects:
      [ if val1 && val2
          then pure $ Just CalculateDays
          else pure Nothing
      ]
    }
    where lb = strToMaybeInt $ targetValue ev
          vi (Visa1 v1) l = Visa1 { lower: l, upper: v1.upper, id: v1.id, msg: v1.msg }
          vi x y = const x y
          currentUpper = getUpperBound st.visa
          val1 = compareByInt lb currentUpper
          val2 = nonNegativeOrEmpty lb
          errMessage =
              if val1
                  then if val2 then emptyMsg else toMsg "X should be positive"
                  else toMsg "X should be less than Y"

foldp (ProvideUpper ev) (State st) = 
    { state: State st { visa = setVisaMsg errMessage (vi st.visa lb) }
    , effects:
      [ if val1 && val2
          then pure $ Just CalculateDays
          else pure Nothing
      ]
    }
    where lb = strToMaybeInt $ targetValue ev
          vi (Visa1 v1) u = Visa1 { lower: v1.lower, upper: u, id: v1.id, msg: v1.msg }
          vi x y = const x y
          currentLower = getLowerBound st.visa
          val1 = compareByInt currentLower lb
          val2 = nonNegativeOrEmpty lb
          errMessage =
              if val1
                  then if val2 then emptyMsg else toMsg "Y should be positive"
                  else toMsg "X should be less than y"

foldp (ChangeRangeError attr invalid pid str) (State st) = noEffects $
    State st { userRanges = map (setErrorById invalid pid)
                          $ map (setUserInputById attr pid str) st.userRanges
             }

foldp (ChangeRange attr pid d str) (State st) = 
  { state: State st { ranges = newRanges
                    , userRanges = newUserRanges
                    }
  , effects:
    [ do neldt <- liftEff' $ nowDateTime
         case neldt of
           Left err -> pure Nothing
           Right nldt -> do
             let nd = date $ extract $ nldt
             DateRange dr <- pure $ unsafePartial $ fromJust $ head $ filter (byId pid) newRanges
             res1 <- pure $ case attr of
                              Start -> nonNegativeOrEmpty $ calculateDiff dr.end (MaybeDate {unMaybeDate: Just d})
                              End -> nonNegativeOrEmpty $ calculateDiff (MaybeDate {unMaybeDate: Just d}) dr.start
             xday <- liftEff' $ getDate $ fromDateTime $ toDateTime d
            
             day <- pure $ justShowDatePartE xday "01"
             out <- pure $ wrapToString (toDateTime d) day
             _ <- liftEff' $ log $ show res1
             if res1
                 then pure $ Just CalculateDays
                 else pure $ Just $ ChangeRangeError attr StartGreaterThanEnd pid out

    ]
  }
  where newRanges = map (assignById attr pid d) st.ranges
        newUserRanges = map (setUserInputById attr pid str) st.userRanges
        byId x (DateRange y) = x == y.id

                 

foldp (GetNow) (State st) =
  { state: State st 
  , effects:
    [ do nd <- liftEff' $ nowDateTime
         case nd of
	   Left err -> pure Nothing
	   Right d -> do let xd = extract d
                         day <- liftEff' $ getDate $ fromDateTime xd
                         pure $ Just $ AddPeriod xd $ justShowDatePartE day "01"
    ]
  }

foldp (AddPeriod nd nda) (State st) = noEffects $
  State st { userRanges = newPeriod nd : st.userRanges
           , nextId = st.nextId + 1
           , ranges = nr2 : st.ranges
           , today = MaybeDate { unMaybeDate: Just (date nd) }
           }
  where
  newPeriod nd2 = UserRange
                  { id: st.nextId
                  , start: (wrapDate $ wrapToString nd2 nda)
                  , end: (wrapDate $ wrapToString nd2 nda)
                  , msg: emptyMsg
                  }
  nr2 = zeroDateRange (date nd) st.nextId

foldp (RemovePeriod pid) (State st) = 
  { state: State st { userRanges = filterUserRangeById (/=) pid st.userRanges, ranges = filterDateRangeById (/=) pid st.ranges }
  , effects:
    [ pure $ Just CalculateDays
    ]
  }

{-foldp (CalculateDays) (State st) = noEffects $
  State st { result = res }
  where lb = fromMaybeInt $ getLowerBound st.visa
        ub = fromMaybeInt $ getUpperBound st.visa
        (MaybeDate mbtoday) = st.today
        td = unsafePartial $ fromJust mbtoday.unMaybeDate
        res = show $ algorithm td lb ub $ map dateRangeToPeriod st.ranges-}

foldp (CalculateDays) (State st) = 
  { state: State st { result = res }
  , effects:
    [ do let xss = filter (notEq z2) dranges
             fs = filter (not $ rangeInPast td) xss
             z2 = emptyPeriod
             fds = concatMap periodToDates fs 
             fps = map (dateToFuturePoint xss lb ub) fds
             rps = map futureToResultPoint fps
             rps' = dropWhile (not $ limitReached) rps
             -- dateToFuturePoint debug --
             ps = filter (not $ rangeInFuture td) xss
             ps' = mergePeriodStart startDay $ mergePeriodEnd td ps
             ps'' = filter (not $ rangeInPast startDay) ps'
             startDay = addDays td (negate ub)
             alldays = concatMap periodToDates ps''
         _ <- liftEff' $ log $ "====== START ======="
         _ <- liftEff' $ log $ "Today: " <> (show st.today)
         _ <- liftEff' $ log $ "Lower bound: " <> (show lb)
         _ <- liftEff' $ log $ "Upper bound: " <> (show ub)
         _ <- liftEff' $ log $ "DateRange length: " <> (show $ length dranges)

         _ <- liftEff' $ log $ "DateRange length (in past): " <> (show $ length fs)
         _ <- liftEff' $ log $ "DateRange length (w/o dummy period): " <> (show $ length fs)
         _ <- liftEff' $ log $ "Date length (in past, w/o dummy): " <> (show $ length fps)
         _ <- liftEff' $ log $ "Date length (future points): " <> (show $ length fds)
         _ <- liftEff' $ log $ "1st: ResultPoint: " <> (show $ unsafePartial $ fromJust $ head rps)
         _ <- liftEff' $ log $ "ResultPoint (before limit reached): " <> (show $ length rps')
         _ <- liftEff' $ log $ "dateToFuturePoint: Not in future: " <> (show $ length ps)
         _ <- liftEff' $ traverse_ log $ map (append "dateToFuturePoint: (not in future) ps: ") $ map show ps
         _ <- liftEff' $ traverse_ log $ map (append "dateToFuturePoint: (not in future, merged) ps': ") $ map show ps'
         _ <- liftEff' $ log $ "dateToFuturePoint: startDay: " <> (show startDay)
         _ <- liftEff' $ log $ "dateToFuturePoint: not in future intersects with upper bound in the past: " <> (show $ length ps'')
         _ <- liftEff' $ traverse_ log $ map (append "dateToFuturePoint: ps'': ") $ map show ps''
         _ <- liftEff' $ log $ "dateToFuturePoint: all days: " <> (show $ length alldays)
         -- _ <- liftEff' $ traverse_ log $ map (append "dateToFuturePoint: alldays: ") $ map show alldays
         _ <- liftEff' $ log $ "1st FuturePoint: " <> (show $ unsafePartial $ fromJust $ head fps)

         _ <- liftEff' $ log $ "====== END ======="

         pure Nothing
    ]
  }
  where lb = fromMaybeInt $ getLowerBound st.visa
        ub = fromMaybeInt $ getUpperBound st.visa
        (MaybeDate mbtoday) = st.today
        td = unsafePartial $ fromJust mbtoday.unMaybeDate
        res = show $ algorithm td lb ub dranges
        dranges = map dateRangeToPeriod $ overlap6 st.ranges
        -- TEST DATA --
        testMaybeDate1 = makeMaybeDate $ unsafeMakeDate 2018 5 1
        testMaybeDate2 = makeMaybeDate $ unsafeMakeDate 2018 5 15
        testMaybeDate3 = makeMaybeDate $ unsafeMakeDate 2018 5 30
        testMaybeDate4 = makeMaybeDate $ unsafeMakeDate 2018 6 8
        testMaybeDate5 = makeMaybeDate $ unsafeMakeDate 2018 6 18
        testDateRange1 = makeDateRange testMaybeDate1 testMaybeDate2 (negate 2)
        testDateRange2 = makeDateRange testMaybeDate1 testMaybeDate3 (negate 3)
        testDateRange3 = makeDateRange testMaybeDate4 testMaybeDate5 (negate 4)
        


ixUserRange :: UserRange -> Int
ixUserRange (UserRange x) = x.id

filterUserRangeById :: (Int -> Int -> Boolean) -> Int -> Array UserRange -> Array UserRange
filterUserRangeById pred pid visas = filter ((pred pid) <<< ixUserRange) visas

ixDateRange :: DateRange -> Int
ixDateRange (DateRange x) = x.id

filterDateRangeById :: (Int -> Int -> Boolean) -> Int -> Array DateRange -> Array DateRange
filterDateRangeById pred pid visas = filter ((pred pid) <<< ixDateRange) visas

data Attr = Start | End
data Validation = InvalidFormatStart | InvalidFormatEnd
                | StartGreaterThanEnd
                | IsNanLower | IsNanUpper | LowerGreaterThanUpper

setErrorById :: Validation -> Int -> UserRange -> UserRange
setErrorById invalid pid (UserRange v) =
    if v.id == pid
      then case invalid of
             InvalidFormatStart -> UserRange
                                   { start: (setWidgetMsg v.start edgeMsg)
                                   , end: v.end
                                   , id: v.id
                                   , msg: v.msg
                                   }
             InvalidFormatEnd -> UserRange
                                 { start: v.start
                                 , end: (setWidgetMsg v.end edgeMsg)
                                 , id: v.id
                                 , msg: v.msg
                                 }
             StartGreaterThanEnd -> UserRange
                                    { start: v.start, end: v.end, id: v.id
                                    , msg: toMsg "Start date should be less than end date" }
             _ -> UserRange v
                                   
      else (UserRange v)
    where edgeMsg = toMsg "Format should be YYYY-MM-DD"

setUserInputById :: Attr -> Int -> String -> UserRange -> UserRange
setUserInputById Start pid new (UserRange v) =
    if v.id == pid
      then  (UserRange { start: DateWidget { widgetDate: new, widgetMsg : emptyMsg }
                       , end: v.end
                       , id: v.id
                       , msg: emptyMsg
                       })
      else (UserRange v)

setUserInputById End pid new (UserRange v) =
    if v.id == pid
      then (UserRange { start: v.start
                      , end: DateWidget { widgetDate: new, widgetMsg: emptyMsg }
                      , id: v.id
                      , msg: emptyMsg
                      })
      else (UserRange v)

assignById :: Attr -> Int -> Date -> DateRange -> DateRange
assignById Start pid new (DateRange v) =
    if v.id == pid
      then (DateRange { start: maynew
                      , end: v.end
                      , id: v.id
                      , diff: calculateDiff maynew v.end
                      })
      else (DateRange v)
    where maynew = MaybeDate { unMaybeDate: Just new }

assignById End pid new (DateRange v) =
    if v.id == pid
    then (DateRange { start: v.start
                    , end: maynew
                    , id: v.id
                    , diff: calculateDiff v.start maynew
                    })                    
    else (DateRange v)
  where maynew = MaybeDate { unMaybeDate: Just new }

wrapDate :: String -> DateWidget
wrapDate nd = DateWidget { widgetDate : nd, widgetMsg : emptyMsg }

wrapToString :: DateTime -> String -> String
wrapToString d day = f (fromDateTime d) day
  where f :: JSDate -> String -> String
        f x1 y1 = (g $ getUTCFullYear x1) <> "-" <> (g $ inc $ getUTCMonth x1) <> "-" <> (h day)
        g :: Number -> String
        g x2 = h $ toString x2
        h :: String -> String
        h x3 = if (String.length x3 == 1) then "0" <> x3 else x3        


dateRegex :: Regex
dateRegex =
  unsafePartial
    case regex "^\\d{4}-\\d{2}-\\d{2}$" noFlags of
      Right r -> r

inc :: Number -> Number
inc x = x + 1.0

whatError :: Attr -> Validation
whatError Start = InvalidFormatStart
whatError End = InvalidFormatEnd

