module Styles exposing (..)

import Css exposing (..)
import Html.Styled exposing (Attribute)
import Html.Styled.Attributes exposing (css)


boxStyles : Bool -> Attribute msg
boxStyles clicked =
    let
        commonStyles =
            [ width (px 100)
            , height (px 100)
            , border3 (px 10) solid (rgb 11 14 17)
            ]
    in
    if clicked then
        css <| commonStyles ++ [ backgroundColor (rgb 220 210 145) ]

    else
        css commonStyles
