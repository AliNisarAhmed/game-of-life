module Rules exposing
    ( Born
    , Description
    , Rule(..)
    , RuleLabels(..)
    , Survive
    , defaultRule
    , gameOfLifeRule
    , getRule
    , getRuleInfo
    , highLifeRule
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
    | HighLife


type alias Description =
    String


defaultRule : Rule
defaultRule =
    gameOfLifeRule


gameOfLifeRule : Rule
gameOfLifeRule =
    Rule [ 3 ] [ 2, 3 ]


highLifeRule : Rule
highLifeRule =
    Rule [ 3, 6 ] [ 2, 3 ]


rulesToString : RuleLabels -> String
rulesToString rules =
    case rules of
        GameOfLife ->
            "Conway's Game of Life"

        HighLife ->
            "HighLife"


rulesDict : Dict RuleLabels ( Rule, Description )
rulesDict =
    Dict.fromList rulesToString rulesList


getRule : RuleLabels -> Maybe Rule
getRule label =
    Maybe.map Tuple.first <| Dict.get rulesToString label rulesDict


ruleNames : List RuleLabels
ruleNames =
    Dict.keys rulesDict


rulesList : List ( RuleLabels, ( Rule, Description ) )
rulesList =
    [ ( GameOfLife, ( gameOfLifeRule, gameOfLifeDescription ) )
    , ( HighLife, ( highLifeRule, halfLifeDescription ) )
    ]


gameOfLifeDescription : Description
gameOfLifeDescription =
    "The original game of life rules as formulated by Mr. John Conway. " ++ ruleDescription gameOfLifeRule


halfLifeDescription : Description
halfLifeDescription =
    "HighLife is a cellular automator similar to Conway's Game of Life. It was devised in 1994 by Nathan Thompson. " ++ ruleDescription highLifeRule


ruleDescription : Rule -> Description
ruleDescription (Rule born survive) =
    let
        b =
            String.concat <| List.map String.fromInt born

        s =
            String.concat <| List.map String.fromInt survive
    in
    "\"B" ++ b ++ "\" means that a cell is born if exactly 3 of its neighbours are alive. \"S" ++ s ++ "\" means that a cell survives to the next generation if exactly 2 or 3 of its neighbours are alive, else it dies"


getRuleInfo : Rule -> String
getRuleInfo ((Rule born survive) as rule) =
    let
        bornString =
            "B" ++ listOfNumbersToString born

        surviveString =
            "S" ++ listOfNumbersToString survive

        ruleString =
            bornString ++ surviveString
    in
    if rule == gameOfLifeRule then
        "Conway " ++ ruleString

    else
        "HighLife " ++ ruleString


listOfNumbersToString : List Int -> String
listOfNumbersToString list =
    list
        |> List.map (\n -> String.fromInt n)
        |> String.concat
