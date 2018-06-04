module App.State where

import App.Internal.UserRange
import App.Internal.ErrorMsg
import App.Internal.DateWidget
import App.Internal.Period
import App.Internal.MaybeDate
import App.Internal.MaybeInt
import App.Internal.DateRange

import Prelude (class Show, show, ($), (<>), (<$>), (<*>))
import Data.Enum
import Data.Eq
import App.Config (config)
import App.Routes (Route, match)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Foreign (toForeign)
import Data.Foreign.Class (class Encode)
import Data.Foreign.Generic (defaultOptions, genericEncode)
import Data.Newtype (class Newtype)
import Data.Maybe (Maybe(..), fromJust)
import Data.Date  (Date(..), diff, canonicalDate)

import Data.DateTime
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

-- | Whole State | --

newtype State = State
  { title :: String
  , route :: Route
  , loaded :: Boolean
  , userRanges :: Array UserRange
  , today :: MaybeDate
  , visa :: Visa
  , ranges :: Array DateRange
  , nextId :: Int
  , result :: String
  }

-- State instances 
derive instance genericState :: Generic State _
derive instance newtypeState :: Newtype State _

instance showState :: Show State where show = genericShow

-- State init function
init ::String -> State
init url = State
  { title: config.title
  , route: match url
  , loaded: false
  , userRanges: []
  , today: MaybeDate { unMaybeDate: Just yarrr }
  , visa: emptyVisa
  , ranges: []
  , nextId: 0
  , result: ""
  }

-- | Visa | --
data Visa = Visa1
  { lower :: MaybeInt
  , upper :: MaybeInt
  , id :: Int
  , msg :: ErrorMsg
  } | Visa2
  { startDate :: MaybeDate
  , lower :: MaybeInt
  , upper :: MaybeInt
  , id :: Int
  , msg :: ErrorMsg
  } | Visa3
  { endDate :: MaybeDate
  , lower :: MaybeInt
  , upper :: MaybeInt
  , id :: Int
  , msg :: ErrorMsg
  } | Visa4
  { startDate :: MaybeDate
  , period :: MaybeInt
  , id :: Int
  , msg :: ErrorMsg
  }

-- Visa instances
derive instance genericVisa :: Generic Visa _
-- derive instance newtypeVisa :: Newtype Visa _

instance showVisa :: Show Visa where show = genericShow

instance encodeVisa :: Encode Visa where
  encode = genericEncode (defaultOptions { unwrapSingleConstructors = true })

emptyVisa :: Visa
emptyVisa = Visa1 { lower: justZero, upper: justZero, id: 1, msg: emptyMsg }

-- helpers
getLowerBound :: Visa -> MaybeInt
getLowerBound (Visa1 v) = v.lower
getLowerBound (Visa2 v) = v.lower
getLowerBound (Visa3 v) = v.lower
getLowerBound (Visa4 v) = toInt 365

getUpperBound :: Visa -> MaybeInt
getUpperBound (Visa1 v) = v.upper
getUpperBound (Visa2 v) = v.upper
getUpperBound (Visa3 v) = v.upper
getUpperBound (Visa4 v) = emptyInt

setVisaMsg :: ErrorMsg -> Visa -> Visa
setVisaMsg m (Visa1 v) = Visa1 v { msg = m }
setVisaMsg m (Visa2 v) = Visa2 v { msg = m }
setVisaMsg m (Visa3 v) = Visa3 v { msg = m }
setVisaMsg m (Visa4 v) = Visa4 v { msg = m }

getVisaMsg :: Visa -> ErrorMsg
getVisaMsg (Visa1 v) = v.msg
getVisaMsg (Visa2 v) = v.msg
getVisaMsg (Visa3 v) = v.msg
getVisaMsg (Visa4 v) = v.msg