module Patterns exposing
    ( BoxStatus(..)
    , Coordinates
    , Pattern(..)
    , default
    , defaultPattern
    , getPattern
    , patternDict
    , patternList
    )

import Dict exposing (Dict)


type BoxStatus
    = Occupied
    | UnOccupied


type alias PatternFunction =
    Int -> Int -> Dict Coordinates BoxStatus


type alias Coordinates =
    ( Int, Int )


defaultPattern : Pattern
defaultPattern =
    Oscillator


default : PatternFunction
default =
    oscillator


oscillator : PatternFunction
oscillator width height =
    let
        midWidth =
            width // 2

        midHeight =
            height // 2
    in
    Dict.fromList
        [ ( ( midHeight - 1, midWidth ), Occupied )
        , ( ( midHeight, midWidth ), Occupied )
        , ( ( midHeight + 1, midWidth ), Occupied )
        ]


toad : PatternFunction
toad width height =
    let
        midHeight =
            height // 2

        midWidth =
            width // 2
    in
    Dict.fromList
        [ ( ( midHeight, midWidth ), Occupied )
        , ( ( midHeight, midWidth + 1 ), Occupied )
        , ( ( midHeight, midWidth + 2 ), Occupied )
        , ( ( midHeight + 1, midWidth - 1 ), Occupied )
        , ( ( midHeight + 1, midWidth ), Occupied )
        , ( ( midHeight + 1, midWidth + 1 ), Occupied )
        ]


glider : PatternFunction
glider width height =
    let
        midWidth =
            width // 2

        midHeight =
            height // 2
    in
    Dict.fromList
        [ ( ( midHeight - 1, midWidth ), Occupied )
        , ( ( midHeight, midWidth - 2 ), Occupied )
        , ( ( midHeight, midWidth ), Occupied )
        , ( ( midHeight + 1, midWidth - 1 ), Occupied )
        , ( ( midHeight + 1, midWidth ), Occupied )
        ]


dieHard : PatternFunction
dieHard width height =
    let
        midWidth =
            width // 2

        midHeight =
            height // 2
    in
    Dict.fromList
        [ ( ( midHeight, midWidth - 3 ), Occupied )
        , ( ( midHeight, midWidth - 2 ), Occupied )
        , ( ( midHeight, midWidth + 3 ), Occupied )
        , ( ( midHeight + 1, midWidth - 2 ), Occupied )
        , ( ( midHeight + 1, midWidth + 2 ), Occupied )
        , ( ( midHeight + 1, midWidth + 3 ), Occupied )
        , ( ( midHeight + 1, midWidth + 4 ), Occupied )
        ]


rPentomino : PatternFunction
rPentomino width height =
    let
        midWidth =
            width // 2

        midHeight =
            height // 2
    in
    Dict.fromList
        [ ( ( midHeight - 1, midWidth ), Occupied )
        , ( ( midHeight - 1, midWidth + 1 ), Occupied )
        , ( ( midHeight, midWidth - 1 ), Occupied )
        , ( ( midHeight, midWidth ), Occupied )
        , ( ( midHeight + 1, midWidth ), Occupied )
        ]


acorn : PatternFunction
acorn width height =
    let
        midWidth =
            width // 2

        midHeight =
            height // 2
    in
    Dict.fromList
        [ ( ( midHeight - 2, midWidth + 18 ), Occupied )
        , ( ( midHeight - 1, midWidth + 20 ), Occupied )
        , ( ( midHeight, midWidth + 17 ), Occupied )
        , ( ( midHeight, midWidth + 18 ), Occupied )
        , ( ( midHeight, midWidth + 21 ), Occupied )
        , ( ( midHeight, midWidth + 22 ), Occupied )
        , ( ( midHeight, midWidth + 23 ), Occupied )
        ]


talker : PatternFunction
talker width height =
    let
        midWidth =
            width // 2

        midHeight =
            height // 2
    in
    Dict.fromList
        [ ( ( midHeight - 2, midWidth - 2 ), Occupied )
        , ( ( midHeight - 1, midWidth ), Occupied )
        , ( ( midHeight, midWidth - 3 ), Occupied )
        , ( ( midHeight, midWidth - 2 ), Occupied )
        , ( ( midHeight, midWidth - 1 ), Occupied )
        , ( ( midHeight, midWidth ), Occupied )
        , ( ( midHeight, midWidth + 1 ), Occupied )
        , ( ( midHeight, midWidth + 2 ), Occupied )
        , ( ( midHeight, midWidth + 3 ), Occupied )
        ]


gosperGliderGun : PatternFunction
gosperGliderGun width height =
    Dict.fromList
        [ ( ( 1, 25 ), Occupied )
        , ( ( 2, 23 ), Occupied )
        , ( ( 2, 25 ), Occupied )
        , ( ( 3, 13 ), Occupied )
        , ( ( 3, 14 ), Occupied )
        , ( ( 3, 21 ), Occupied )
        , ( ( 3, 22 ), Occupied )
        , ( ( 3, 35 ), Occupied )
        , ( ( 3, 36 ), Occupied )
        , ( ( 4, 12 ), Occupied )
        , ( ( 4, 16 ), Occupied )
        , ( ( 4, 21 ), Occupied )
        , ( ( 4, 22 ), Occupied )
        , ( ( 4, 35 ), Occupied )
        , ( ( 4, 36 ), Occupied )
        , ( ( 5, 1 ), Occupied )
        , ( ( 5, 2 ), Occupied )
        , ( ( 5, 11 ), Occupied )
        , ( ( 5, 17 ), Occupied )
        , ( ( 5, 21 ), Occupied )
        , ( ( 5, 22 ), Occupied )
        , ( ( 6, 1 ), Occupied )
        , ( ( 6, 2 ), Occupied )
        , ( ( 6, 11 ), Occupied )
        , ( ( 6, 15 ), Occupied )
        , ( ( 6, 17 ), Occupied )
        , ( ( 6, 18 ), Occupied )
        , ( ( 6, 23 ), Occupied )
        , ( ( 6, 25 ), Occupied )
        , ( ( 7, 11 ), Occupied )
        , ( ( 7, 17 ), Occupied )
        , ( ( 7, 25 ), Occupied )
        , ( ( 8, 12 ), Occupied )
        , ( ( 8, 16 ), Occupied )
        , ( ( 9, 13 ), Occupied )
        , ( ( 9, 14 ), Occupied )
        ]


patternList : List ( String, Pattern )
patternList =
    [ ( patternToString Oscillator, Oscillator )
    , ( patternToString Glider, Glider )
    , ( patternToString DieHard, DieHard )
    , ( patternToString RPentomino, RPentomino )
    , ( patternToString Acorn, Acorn )
    , ( patternToString Toad, Toad )
    , ( patternToString Talker, Talker )
    , ( patternToString GosperGliderGun, GosperGliderGun )
    ]


patternDict : Dict String PatternFunction
patternDict =
    Dict.fromList
        [ ( patternToString Oscillator, oscillator )
        , ( patternToString Glider, glider )
        , ( patternToString DieHard, dieHard )
        , ( patternToString RPentomino, rPentomino )
        , ( patternToString Acorn, acorn )
        , ( patternToString Toad, toad )
        , ( patternToString Talker, talker )
        , ( patternToString GosperGliderGun, gosperGliderGun )
        ]


type Pattern
    = Oscillator
    | Glider
    | DieHard
    | Acorn
    | Talker
    | RPentomino
    | Toad
    | GosperGliderGun


patternToString : Pattern -> String
patternToString ptr =
    case ptr of
        Oscillator ->
            "Oscillator"

        Glider ->
            "Glider"

        DieHard ->
            "DieHard"

        RPentomino ->
            "RPentomino"

        Acorn ->
            "Acorn"

        Talker ->
            "Talker"

        GosperGliderGun ->
            "GosperGliderGun"

        _ ->
            "Toad"


getPattern : Pattern -> PatternFunction
getPattern ptr =
    Maybe.withDefault default <| Dict.get (patternToString ptr) patternDict
