module Patterns exposing
    ( BoxStatus(..)
    , Coordinates
    , Pattern(..)
    , default
    , getPattern
    )

import Dict exposing (Dict)


type BoxStatus
    = Occupied
    | UnOccupied


type alias PatternFunction =
    Int -> Int -> Dict Coordinates BoxStatus


type alias Coordinates =
    ( Int, Int )


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
        [ ( ( midHeight - 2, midWidth - 2 ), Occupied )
        , ( ( midHeight - 1, midWidth ), Occupied )
        , ( ( midHeight, midWidth - 3 ), Occupied )
        , ( ( midHeight, midWidth - 2 ), Occupied )
        , ( ( midHeight, midWidth + 1 ), Occupied )
        , ( ( midHeight, midWidth + 2 ), Occupied )
        , ( ( midHeight, midWidth + 3 ), Occupied )
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
        ]


type Pattern
    = Oscillator
    | Glider
    | DieHard
    | Acorn
    | Talker
    | RPentomino
    | Toad


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

        _ ->
            "Toad"


getPattern : Pattern -> PatternFunction
getPattern ptr =
    Maybe.withDefault default <| Dict.get (patternToString ptr) patternDict
