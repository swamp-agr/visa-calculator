module App.Internal.DateWidget where

import App.Internal.ErrorMsg

import Prelude (class Show, show, ($), (<>), (<$>), (<*>))
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Foreign (toForeign)
import Data.Foreign.Class (class Encode)
import Data.Foreign.Generic (defaultOptions, genericEncode)
import Data.Newtype (class Newtype)

-- | DateWidget | --

newtype DateWidget = DateWidget
  { widgetDate :: String, widgetMsg :: ErrorMsg }

-- DateWidget instances
derive instance genericDateWidget :: Generic DateWidget _
derive instance newtypeDateWidget :: Newtype DateWidget _

instance showDateWidget :: Show DateWidget where show = genericShow

instance encodeDateWidget :: Encode DateWidget where
  encode (DateWidget val) = toForeign $ (val.widgetDate) <> "T00:00:00.000Z"

-- helpers
setWidgetMsg :: DateWidget -> ErrorMsg -> DateWidget
setWidgetMsg (DateWidget v) msg = DateWidget { widgetDate: v.widgetDate, widgetMsg: msg }
