module MyGridModule exposing (..)

import Color exposing (Color)
import Html exposing (Html)


type MyGrid
    = MyGrid GridOptions


type alias GridOptions =
    { rows : Int
    , columns : Int
    , cellSize : Float
    }


type Msg
    = MyGridMessage Coordinates


type alias Coordinates =
    ( Int, Int )


type GridCell
    = GridCell Coordinates (Html Msg)
