module Styles exposing (..)

import Color as C
import Element as E exposing (Attr, Attribute, Color, explain)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Svg
import Svg.Attributes as SA



--
-- explain =
--     E.explain <| Debug.todo
---- COLORS ----


black : C.Color
black =
    C.black


gray : C.Color
gray =
    C.gray


white : Color
white =
    E.rgb255 255 255 255


backgroundColor : Color
backgroundColor =
    E.rgb255 24 36 43


primaryColor : Color
primaryColor =
    E.rgb255 87 22 206



--- GRID COLORS ----


occupiedColor : C.Color
occupiedColor =
    C.rgb255 0 0 0


unOccupiedColor : C.Color
unOccupiedColor =
    C.rgb255 245 245 245


container : List (Attribute msg)
container =
    [ E.height E.fill
    , E.width E.fill
    , Background.color backgroundColor
    ]


layout : List (Attribute msg)
layout =
    [ E.height E.fill
    , E.width E.fill
    ]


sidebarStyles : List (Attribute msg)
sidebarStyles =
    [ E.height <| E.fill
    , E.width (E.fill |> E.minimum 80)
    , E.spaceEvenly
    , Border.widthEach { bottom = 0, right = 1, top = 0, left = 0 }
    , Border.color primaryColor
    , E.paddingXY 0 10
    , E.centerX
    , E.centerY
    , Border.shadow { offset = ( 2.0, 2.0 ), size = 2.0, blur = 10.0, color = primaryColor }
    ]


sidebarRowStyles : List (Attribute msg)
sidebarRowStyles =
    [ E.centerX, E.paddingXY 0 20 ]


sidebarColumnStyles : List (Attribute msg)
sidebarColumnStyles =
    [ E.spacingXY 0 15 ]


sidebarIconStyles : List (Attribute msg)
sidebarIconStyles =
    [ E.centerX ]


bookIconStyles : List (Attribute msg)
bookIconStyles =
    sidebarIconStyles ++ [ E.paddingXY 0 10 ]


settingsIconStyles : List (Attribute msg)
settingsIconStyles =
    sidebarIconStyles ++ [ E.alignTop, E.paddingXY 0 10 ]


gridContainer : List (Attribute msg)
gridContainer =
    [ E.spacing 0
    , E.centerX
    , E.centerY
    , E.width <| E.fill
    , E.height E.fill
    ]


uiStyles : List (Attribute msg)
uiStyles =
    [ E.centerX, E.centerY, E.spacingXY 0 10 ]


speedControlStyles : List (Attribute msg)
speedControlStyles =
    [ E.alignLeft
    , E.paddingEach { top = 0, bottom = 0, left = 0, right = 20 }
    , E.width <| E.fillPortion 2
    ]


gridLayout : List (Attribute msg)
gridLayout =
    [ E.centerY
    , E.centerX
    , E.width E.fill
    , E.height E.fill
    ]


gridStyles : List (Attribute msg)
gridStyles =
    [ E.centerY
    , E.centerX
    ]


bookStyles : List (Attribute msg)
bookStyles =
    [ E.spaceEvenly
    , E.paddingXY 30 10
    , Border.width 10
    , E.width E.fill
    , E.height E.fill
    , Background.color <| E.rgb255 123 123 123
    ]



---- ICON STYLES ----


iconStyles : List (Svg.Attribute msg)
iconStyles =
    [ SA.color "#ffffff"
    ]


hiddenIcon : List (Attribute msg)
hiddenIcon =
    [ E.transparent True ]


patternDisplayStyles : List (Attribute msg)
patternDisplayStyles =
    [ E.centerY
    , E.centerX
    , E.width E.fill
    , E.alignLeft
    , Font.color white
    ]


textStyles : List (Attribute msg)
textStyles =
    [ E.centerX
    , E.centerY
    , Font.color white
    , Font.size 16
    ]


heading : List (Attribute msg)
heading =
    [ E.alignLeft
    , Font.color white
    , Font.size 24
    ]


subHeading : List (Attribute msg)
subHeading =
    [ E.alignLeft
    , Font.color primaryColor
    , Font.size 16
    ]


paragraph : List (Attribute msg)
paragraph =
    [ E.alignLeft
    , Font.color primaryColor
    , Font.size 14
    ]


settingsStyles : List (Attribute msg)
settingsStyles =
    [ E.spaceEvenly
    , E.paddingXY 10 10
    , Border.width 10
    , E.width E.fill
    , E.height E.fill
    , Background.color <| E.rgb255 123 123 123
    ]


ruleRowStyles : List (Attribute msg)
ruleRowStyles =
    [ E.width E.fill, E.mouseOver [ Background.color <| E.rgba255 180 180 180 0.4 ] ]


ruleElementStyles : List (Attribute msg)
ruleElementStyles =
    [ E.width E.fill ]
