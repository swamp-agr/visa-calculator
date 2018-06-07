module App.View.Layout where

import App.View.Homepage as Homepage
import App.View.NotFound as NotFound
import App.Routes (Route(NotFound, Home))
import App.State (State(..))
import App.Events (Event)
import CSS (CSS, fromString, (?), fontSize, marginTop, marginRight, marginLeft, px, value, key, color, backgroundColor, padding, borderRadius, minWidth, width, display, inline)
import CSS.Common (auto)
import CSS.Border (border, solid)
import CSS.Size (nil, pct)
import CSS.TextAlign (center, textAlign)
import CSS.Text (textDecoration, noneTextDecoration, letterSpacing)
import CSS.Text.Transform (textTransform, uppercase)
import Color (rgb)
import Control.Bind (discard)
import Data.Function (($), (#))
import Pux.DOM.HTML (HTML, style)
import Text.Smolder.HTML (div)
import Text.Smolder.HTML.Attributes (className)
import Text.Smolder.Markup ((!))

view :: State -> HTML Event
view (State st) =
  div ! className "app" $ do
    style css

    case st.route of
      (Home) -> Homepage.view (State st)
      (NotFound url) -> NotFound.view (State st)

css :: CSS
css = do
  let green = rgb 14 196 172
      blue = rgb 14 154 196
      white = rgb 250 250 250

  fromString "body" ? do
    backgroundColor (rgb 0 20 30)
    key (fromString "font-family") (value "-apple-system,BlinkMacSystemFont,\"Segoe UI\",Roboto,Oxygen-Sans,Ubuntu,Cantarell,\"Helvetica Neue\",sans-serif")
    color white
    textAlign center

  fromString "h1" ? do
    fontSize (48.0 #px)
    marginTop (48.0 #px)
    textTransform uppercase
    letterSpacing (6.0 #px)

  fromString "table" ? do
    borderRadius (2.0 #px) (2.0 #px) (2.0 #px) (2.0 #px)
    marginLeft (auto)
    marginRight (auto)
    padding (6.0 #px) (6.0 #px) (6.0 #px) (6.0 #px)
    textDecoration noneTextDecoration

  fromString ".cell" ? do
    border solid (2.0 #px) green
    color green
    marginRight (10.0 #px)

  fromString ".guide:hover" ? do
    backgroundColor green
    color white

  fromString ".github" ? do
    border solid (2.0 #px) blue
    color blue
    marginLeft (10.0 #px)

  fromString ".github:hover" ? do
    backgroundColor blue
    color white

  fromString ".custom-inline" ? do
    minWidth nil
    width $ (15.0 #pct)
    display inline