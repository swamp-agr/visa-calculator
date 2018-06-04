module App.Internal.ErrorMsg where

import Prelude (class Show, show, ($), (<>), (<$>), (<*>))
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Foreign (toForeign)
import Data.Foreign.Class (class Encode)
import Data.Foreign.Generic (defaultOptions, genericEncode)
import Data.Newtype (class Newtype)

import Data.Maybe (Maybe(..), fromJust)

-- | ErrorMsg | --
-- Simple newtype wrapper for Maybe String

newtype ErrorMsg = ErrorMsg { fromErrorMsg :: Maybe String }

-- ErrorMsg instances
instance showErrorMsg :: Show ErrorMsg where
  show (ErrorMsg val) =
    case val.fromErrorMsg of
      Just x  -> x
      Nothing -> ""

instance encodeErrorMsg :: Encode ErrorMsg where
  encode (ErrorMsg val) = toForeign $ (show val.fromErrorMsg)

-- ErrorMsg helpers
emptyMsg :: ErrorMsg
emptyMsg = ErrorMsg { fromErrorMsg : Nothing }

toMsg :: String -> ErrorMsg
toMsg x = ErrorMsg { fromErrorMsg : Just x }

fromMsg :: ErrorMsg -> Maybe String
fromMsg (ErrorMsg x) = x.fromErrorMsg
