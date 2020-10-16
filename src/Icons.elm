module Icons exposing (..)

import Element as E exposing (Element)
import FeatherIcons as FeatherIcons
import Styles exposing (iconStyles)


travelForwardIcon : Element msg
travelForwardIcon =
    FeatherIcons.chevronRight
        |> FeatherIcons.withSize 20
        |> FeatherIcons.withClass "icon"
        |> FeatherIcons.toHtml iconStyles
        |> E.html


travelBackwardIcon : Element msg
travelBackwardIcon =
    FeatherIcons.chevronLeft
        |> FeatherIcons.withSize 20
        |> FeatherIcons.withClass "icon"
        |> FeatherIcons.toHtml iconStyles
        |> E.html


playIcon : Element msg
playIcon =
    FeatherIcons.play
        |> FeatherIcons.withSize 40
        |> FeatherIcons.withClass "icon"
        |> FeatherIcons.toHtml iconStyles
        |> E.html


pauseIcon : Element msg
pauseIcon =
    FeatherIcons.pause
        |> FeatherIcons.withSize 40
        |> FeatherIcons.withClass "icon"
        |> FeatherIcons.toHtml iconStyles
        |> E.html


decreaseSpeedIcon : Element msg
decreaseSpeedIcon =
    FeatherIcons.chevronsDown
        |> FeatherIcons.withClass "icon"
        |> FeatherIcons.toHtml iconStyles
        |> E.html


increaseSpeedIcon : Element msg
increaseSpeedIcon =
    FeatherIcons.chevronsUp
        |> FeatherIcons.withClass "icon"
        |> FeatherIcons.toHtml iconStyles
        |> E.html


menuIcon : Element msg
menuIcon =
    FeatherIcons.menu
        |> FeatherIcons.withClass "icon"
        |> FeatherIcons.toHtml iconStyles
        |> E.html


crossIcon : Element msg
crossIcon =
    FeatherIcons.x
        |> FeatherIcons.withClass "icon"
        |> FeatherIcons.toHtml iconStyles
        |> E.html


resetIcon : Element msg
resetIcon =
    FeatherIcons.refreshCw
        |> FeatherIcons.withClass "icon"
        |> FeatherIcons.toHtml iconStyles
        |> E.html
