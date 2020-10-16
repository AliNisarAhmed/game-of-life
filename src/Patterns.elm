module Patterns exposing (..)

import GenericDict as Dict exposing (Dict)


type alias Coordinates =
    ( Int, Int )


type alias Board =
    List Coordinates


type alias PatternFunction =
    Int -> Int -> Board


type Pattern
    = Oscillator
    | Glider
    | DieHard
    | Acorn
    | Talker
    | RPentomino
    | Toad
    | GosperGliderGun


defaultPattern : Pattern
defaultPattern =
    Oscillator


defaultPatternFunction : PatternFunction
defaultPatternFunction =
    oscillator


patterns : Dict Pattern PatternFunction
patterns =
    Dict.fromList patternToString
        [ ( Oscillator, oscillator )
        , ( Toad, toad )
        , ( Glider, glider )
        , ( DieHard, dieHard )
        , ( RPentomino, rPentomino )
        , ( Acorn, acorn )
        , ( Talker, talker )
        , ( GosperGliderGun, gosperGliderGun )
        ]


patternKeys : List Pattern
patternKeys =
    Dict.keys patterns


oscillator : PatternFunction
oscillator width height =
    let
        midWidth =
            width // 2

        midHeight =
            height // 2
    in
    [ ( midHeight - 1, midWidth )
    , ( midHeight, midWidth )
    , ( midHeight + 1, midWidth )
    ]


toad : PatternFunction
toad width height =
    let
        midHeight =
            height // 2

        midWidth =
            width // 2
    in
    [ ( midHeight, midWidth )
    , ( midHeight, midWidth + 1 )
    , ( midHeight, midWidth + 2 )
    , ( midHeight + 1, midWidth - 1 )
    , ( midHeight + 1, midWidth )
    , ( midHeight + 1, midWidth + 1 )
    ]


glider : PatternFunction
glider width height =
    let
        midWidth =
            width // 2

        midHeight =
            height // 2
    in
    [ ( midHeight - 1, midWidth )
    , ( midHeight, midWidth - 2 )
    , ( midHeight, midWidth )
    , ( midHeight + 1, midWidth - 1 )
    , ( midHeight + 1, midWidth )
    ]


dieHard : PatternFunction
dieHard width height =
    let
        midWidth =
            width // 2

        midHeight =
            height // 2
    in
    [ ( midHeight, midWidth - 3 )
    , ( midHeight, midWidth - 2 )
    , ( midHeight, midWidth + 3 )
    , ( midHeight + 1, midWidth - 2 )
    , ( midHeight + 1, midWidth + 2 )
    , ( midHeight + 1, midWidth + 3 )
    , ( midHeight + 1, midWidth + 4 )
    ]


rPentomino : PatternFunction
rPentomino width height =
    let
        midWidth =
            width // 2

        midHeight =
            height // 2
    in
    [ ( midHeight - 1, midWidth )
    , ( midHeight - 1, midWidth + 1 )
    , ( midHeight, midWidth - 1 )
    , ( midHeight, midWidth )
    , ( midHeight + 1, midWidth )
    ]


acorn : PatternFunction
acorn width height =
    let
        midWidth =
            width // 2

        midHeight =
            height // 2
    in
    [ ( midHeight - 2, midWidth + 18 )
    , ( midHeight - 1, midWidth + 20 )
    , ( midHeight, midWidth + 17 )
    , ( midHeight, midWidth + 18 )
    , ( midHeight, midWidth + 21 )
    , ( midHeight, midWidth + 22 )
    , ( midHeight, midWidth + 23 )
    ]


talker : PatternFunction
talker width height =
    let
        midWidth =
            width // 2

        midHeight =
            height // 2
    in
    [ ( midHeight - 2, midWidth - 2 )
    , ( midHeight - 1, midWidth )
    , ( midHeight, midWidth - 3 )
    , ( midHeight, midWidth - 2 )
    , ( midHeight, midWidth - 1 )
    , ( midHeight, midWidth )
    , ( midHeight, midWidth + 1 )
    , ( midHeight, midWidth + 2 )
    , ( midHeight, midWidth + 3 )
    ]


gosperGliderGun : PatternFunction
gosperGliderGun width height =
    [ ( 1, 25 )
    , ( 2, 23 )
    , ( 2, 25 )
    , ( 3, 13 )
    , ( 3, 14 )
    , ( 3, 21 )
    , ( 3, 22 )
    , ( 3, 35 )
    , ( 3, 36 )
    , ( 4, 12 )
    , ( 4, 16 )
    , ( 4, 21 )
    , ( 4, 22 )
    , ( 4, 35 )
    , ( 4, 36 )
    , ( 5, 1 )
    , ( 5, 2 )
    , ( 5, 11 )
    , ( 5, 17 )
    , ( 5, 21 )
    , ( 5, 22 )
    , ( 6, 1 )
    , ( 6, 2 )
    , ( 6, 11 )
    , ( 6, 15 )
    , ( 6, 17 )
    , ( 6, 18 )
    , ( 6, 23 )
    , ( 6, 25 )
    , ( 7, 11 )
    , ( 7, 17 )
    , ( 7, 25 )
    , ( 8, 12 )
    , ( 8, 16 )
    , ( 9, 13 )
    , ( 9, 14 )
    ]


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
            "Gosper Glider Gun"

        Toad ->
            "Toad"


getPattern : Pattern -> PatternFunction
getPattern ptr =
    Maybe.withDefault defaultPatternFunction <| Dict.get patternToString ptr patterns


images : Dict Pattern String
images =
    Dict.fromList patternToString
        [ ( Oscillator, "./assets/oscillator.PNG" )
        ]
