module Rules exposing
    ( Born
    , Rule(..)
    , Survive
    , defaultRule
    , gameOfLifeRule
    , halfLifeRule
    , ruleNames
    , rulesDict
    , rulesList
    , rulesToString
    )

import GenericDict as Dict exposing (Dict)


type alias Born =
    List Int


type alias Survive =
    List Int


type Rule
    = Rule Born Survive


type RuleLabels
    = GameOfLife
    | HalfLife


defaultRule : Rule
defaultRule =
    gameOfLifeRule


gameOfLifeRule : Rule
gameOfLifeRule =
    Rule [ 3 ] [ 2, 3 ]


halfLifeRule : Rule
halfLifeRule =
    Rule [ 3, 6 ] [ 2, 3 ]


rulesToString : RuleLabels -> String
rulesToString rules =
    case rules of
        GameOfLife ->
            "Conway's Game of Life"

        HalfLife ->
            "HalfLife"


rulesDict : Dict RuleLabels Rule
rulesDict =
    Dict.fromList rulesToString rulesList


ruleNames : List RuleLabels
ruleNames =
    Dict.keys rulesDict


rulesList : List ( RuleLabels, Rule )
rulesList =
    [ ( GameOfLife, gameOfLifeRule )
    , ( HalfLife, halfLifeRule )
    ]
