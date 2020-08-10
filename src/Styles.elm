module Styles exposing (..)

import Css exposing (..)
import Element as E exposing (Attribute, Element)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input


explain =
    E.explain <| Debug.todo


container : List (Attribute msg)
container =
    [ E.height E.fill
    , E.width E.fill
    ]


layout : List (Attribute msg)
layout =
    [ E.height E.fill
    , E.width E.fill
    ]


sidebarStyles : List (Attribute msg)
sidebarStyles =
    [ E.height <| E.fill
    , Border.width 1
    , E.spaceEvenly
    , E.paddingXY 10 10
    ]


gridContainer : List (Attribute msg)
gridContainer =
    [ E.spaceEvenly
    , E.centerX
    , E.centerY
    , E.width E.fill
    , E.height E.fill
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
    , E.centerX
    , E.centerY
    , Border.width 10
    , E.width E.fill
    , E.height E.fill
    , Background.color <| E.rgb255 123 123 123
    ]
