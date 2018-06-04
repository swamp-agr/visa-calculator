module App.Internal.UserRange where

import App.Internal.ErrorMsg
import App.Internal.DateWidget

import Prelude (class Show, show, ($), (<>), (<$>), (<*>))
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Foreign (toForeign)
import Data.Foreign.Class (class Encode)
import Data.Foreign.Generic (defaultOptions, genericEncode)
import Data.Newtype (class Newtype)

-- | UserRange | --
newtype UserRange = UserRange
  { start :: DateWidget
  , end :: DateWidget
  , id :: Int
  , msg :: ErrorMsg
  }

-- UserRange instances
derive instance genericUserRange :: Generic UserRange _
derive instance newtypeUserRange :: Newtype UserRange _

instance showUserRange :: Show UserRange where show = genericShow

instance encodeUserRange :: Encode UserRange where
encode = genericEncode (defaultOptions { unwrapSingleConstructors = true })