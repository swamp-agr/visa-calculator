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
import Text.Smolder.HTML (div, h1, h3, h4, input, table, tbody, th, tr, td, span, button, br, label, a, ul, li)
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
      div ! className "col-md-3" $ text ""
      col do
        plainCell "col-md-12" $ do
          intro "1. Give me your visa limitations"
        plainCell "col-md-12" $ do
          case (getVisaMsg s.visa) of
            ErrorMsg { fromErrorMsg: Nothing } -> div $ text ""
            ErrorMsg x -> do
              div ! className "label label-default col-sm-7" $ do
                label $ text $ unsafePartial $ fromJust $ x.fromErrorMsg
        div ! className "text-left" $ do
          div ! className "content custom-inline" $ do
            span ! className "custom-inline" $ text "["
            span ! className "custom-inline" $ do
              renderBound Start (getLowerBound s.visa)
            span ! className "custom-inline" $ text "] days in period of ["
            span ! className "custom-inline" $ do
              renderBound End (getUpperBound s.visa)
            span ! className "custom-inline" $ text "] days."
            span ! className "custom-inline" $ text ""
            
        plainCell "col-md-12" $ do
          intro "2. Give me your trips (previous and planned)"
          button #! onClick (const GetNow) ! className "btn btn-primary" $ text "Add Trip"
        div do
          
          br
          for_ s.userRanges renderPeriod

        plainCell "col-md-12 col-xs-12" $ do
          intro $ "RESULT: " <> s.result
        plainCell "col-md-12 col-xs-12" $ do
          label $ text "Next steps:"
          div ! className "text-left" $ text "1. Add calendar widget instead of YYYY-MM-DD manual input"
          div ! className "text-left" $ text "2. Add extra visa types, algorythm and samples"
          div ! className "text-left" $ text "3. Add export/import capabilities"
        br
        plainCell "col-md-12 col-xs-12" $ do
          label $ text "Want more?"
          br
          div ! className "col-sm-auto" $ do
            span ! className "text-left" $ text "Donate by PayPal: "
            a ! href "https://www.paypal.me/swampagr" $ text "https://www.paypal.me/swampagr"
          div ! className "row justify-content-md-center" $ do
            div ! className "col-sm-auto" $ do
              span $ text "Contribute by GitHub: "
              a ! href "http://github.com/swamp-agr/visa-calculator" $ text "http://github.com/swamp-agr/visa-calculator"        
            br
            div ! className "col-sm-auto" $ do
              span $ text "Created by "
              a ! href "https://an-pro.org" $ text "Andrey Prokopenko"
              span $ text " <*> Powered by "
              a ! href "http://purescript-pux.org" $ text "Pux"
      div ! className "col-md-3" $ text ""
  where
  row = div ! className "row-md-12"
  col = div ! className "col-md-6 col-sm-12 col-xs-12"
  plainCell y =
    \x -> div ! className "row" $ do
      div ! className y $ x

  renderPeriod (UserRange p) = row do
    case (fromMsg p.msg) of
      Just x -> div ! className "label label-default" $ text x
      Nothing -> div ! className "row" $ text ""
    div ! className "col-sm-5 col-xs-5" $ renderCalendar Start p.id p.start
    div ! className "col-sm-5 col-xs-5" $ renderCalendar End p.id p.end
    div ! className "col-sm-2 col-xs-2" $ do
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
  renderBound attr x = input #! chooseBound attr ! type' "text" ! className "form-control custom-inline"
                           ! value (show x)