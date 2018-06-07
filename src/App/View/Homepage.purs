module App.View.Homepage where

import Prelude (show)
import App.Events
import App.State
import App.Internal.UserRange
import App.Internal.ErrorMsg
import App.Internal.DateWidget
import App.Internal.Period
import App.Internal.MaybeDate
import App.Internal.MaybeInt
import App.Internal.DateRange

import Control.Bind (discard)
import Data.Function (($), const)
import Pux.DOM.HTML (HTML)
import Pux.DOM.Events
import Text.Smolder.HTML (div, h1, h3, h4, input, table, tbody, th, tr, td, span, button, br, label, a)
import Text.Smolder.HTML.Attributes (className, type', placeholder, value, href)
import Text.Smolder.Markup ((!), (#!), text)
import Data.Foldable (for_)
import Data.DateTime
import Data.Date (Date(..))
import App.Config
import Data.Maybe (Maybe(..), fromJust)
import Data.Monoid ((<>))
import Partial.Unsafe (unsafePartial)

view :: State -> HTML Event
view (State s) = do
  div do
    h1 $ text "Visa Calculator"
    row do
      col do
        row do
          div ! className "col-sm-12" $ do
            button #! onClick (const GetNow) ! className "btn btn-primary" $ text "Add Trip"
        br
        row do
          div ! className "col-sm-12" $ do
            for_ s.userRanges renderPeriod
      div ! className "col-sm-2" $ text ""
      col do
        div do 
          row do
            intro "1. Give me your trips (previous and planned)"
            intro "2. Give me your visa limitations"
            intro "(X days in period of Y days)"
          br
          row do
            case (getVisaMsg s.visa) of
              ErrorMsg { fromErrorMsg: Nothing } -> div $ text ""
              ErrorMsg x -> do
                div ! className "label label-default text-left col-sm-4" $ do
                  label $ text $ unsafePartial $ fromJust $ x.fromErrorMsg
          row do
            div ! className "text-left col-sm-2" $ do
              label $ text "X days:"
              renderBound Start (getLowerBound s.visa)
            div ! className "text-left col-sm-2" $ do
              label $ text "Y days:"
              renderBound End (getUpperBound s.visa)
          br
          row do
            intro "3. ..."
            intro $ "RESULT: " <> s.result
  div ! className "row justify-content-md-center" $ do
    label $ text "Want more?"
    br
    div ! className "col-sm-auto" $ do
      span ! className "text-left" $ text "Donate by PayPal: "
      a ! href "https://www.paypal.me/swampagr" $ text "https://www.paypal.me/swampagr"
  div ! className "row justify-content-md-center" $ do
    div ! className "col-sm-auto" $ do
      span $ text "Contribute by GitHub: "
      a ! href "http://github.com/swamp-agr/visa-calculator" $ text "http://github.com/swamp-agr/visa-calculator"
  where
  row = div ! className "row"
  col = div ! className "col-sm-5"
  renderPeriod (UserRange p) = row do
    case (fromMsg p.msg) of
      Just x -> div ! className "label label-default" $ text x
      Nothing -> div $ text ""
    col $ renderCalendar Start p.id p.start
    col $ renderCalendar End p.id p.end
    div ! className "col-sm-2" $ do
      button #! onClick (const $ RemovePeriod p.id) ! className "btn btn-primary" $ text "Remove"
  renderCalendar attr pid (DateWidget d) = div ! className "form-group" $ do
      div ! className "input-group" $ do
        case (fromMsg d.widgetMsg) of
          Just x -> div ! className "label label-default" $ text x
          Nothing -> div $ text ""
        input #! (chooseEvent attr pid) ! type' "text" ! className "form-control" ! value (d.widgetDate)
  chooseEvent attr pid = onChange (TryParse attr pid)
  chooseBound Start = onChange (ProvideLower)
  chooseBound End   = onChange (ProvideUpper)
  intro x = h3 ! className "text-left" $ text x
  renderBound attr x = input #! chooseBound attr ! type' "text" ! className "form-control"
                           ! value (show x)