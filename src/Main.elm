module Main exposing (..)

import Browser
import CellGrid as CG
import CellGrid.Image as CGI
import CellGrid.Render as CGR
import Color
import Css as Css exposing (..)
import Dict exposing (Dict)
import Html
import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (css, src)
import Html.Styled.Events exposing (onClick)
import List.Extra exposing (andThen)
import Maybe.Extra exposing (isJust)
import Patterns
    exposing
        ( BoxStatus(..)
        , Coordinates
        , Pattern(..)
        , getPattern
        )
import Styles exposing (container, controls)
import Time


type Mode
    = Init
    | Play
    | Pause


type Rule
    = Rule Born Survive


type alias Born =
    List Int


type alias Survive =
    List Int



-- Speed is an Int between 1 and 10 both inclusive


type Speed
    = Speed Int


getSpeed : Speed -> Int
getSpeed (Speed n) =
    n


setSpeed : Int -> Speed -> Speed
setSpeed incr (Speed prev) =
    if prev + incr > 10 || prev + incr < 1 then
        Speed prev

    else
        Speed (prev + incr)



---- MODEL ----


type alias Model =
    { height : Int
    , width : Int
    , cellSize : Float
    , boxes : Dict Coordinates BoxStatus
    , mode : Mode
    , speed : Speed
    }


init : Int -> ( Model, Cmd Msg )
init initialWidth =
    ( { width = initialWidth
      , height = 70
      , cellSize = 10.0
      , boxes = Patterns.default initialWidth 70
      , mode = Init
      , speed = Speed 20
      }
    , Cmd.none
    )



---- UPDATE ----


type Msg
    = NoOp
    | CellGridMsg CGR.Msg
    | Tick Time.Posix
    | ChangeMode Mode
    | ChangeSpeed Int
    | ChangePattern Pattern
    | ChangeWidth Int
    | ChangeHeight Int
    | Reset


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        CellGridMsg cellMsg ->
            if model.mode == Init then
                let
                    coordinates =
                        ( cellMsg.cell.row, cellMsg.cell.column )

                    updatedDict =
                        updateCell coordinates model.boxes
                in
                ( { model | boxes = updatedDict }, Cmd.none )

            else
                ( model, Cmd.none )

        Tick _ ->
            ( { model | boxes = applyGameOfLifeRules model.boxes }, Cmd.none )

        ChangeSpeed n ->
            ( { model | speed = setSpeed n model.speed }, Cmd.none )

        ChangeMode prevMode ->
            let
                newMode =
                    case prevMode of
                        Init ->
                            Play

                        Play ->
                            Pause

                        Pause ->
                            Play
            in
            ( { model | mode = newMode }, Cmd.none )

        ChangePattern ptr ->
            ( { model | boxes = getPattern ptr model.width model.height }, Cmd.none )

        Reset ->
            ( { model | boxes = Patterns.default model.width model.height, mode = Init }, Cmd.none )

        ChangeWidth n ->
            ( { model | width = model.width + n }, Cmd.none )

        ChangeHeight n ->
            ( { model | height = model.height + n }, Cmd.none )



---- VIEW ----


view : Model -> Html.Html Msg
view { height, width, cellSize, mode, boxes, speed } =
    toUnstyled <|
        div [ container ]
            [ drawGrid height width cellSize boxes mode
            , div [ controls ]
                [ button [ onClick (ChangeMode mode) ] [ text <| getModeButtonText mode ]
                , button [ onClick (ChangeSpeed 1) ] [ text <| "Increase Speed" ]
                , text <| String.fromInt <| getSpeed speed
                , button [ onClick (ChangeSpeed -1) ] [ text <| "Decrease Speed" ]
                , button [ onClick (ChangePattern Toad) ] [ text <| "Toad" ]
                , button [ onClick (ChangePattern Glider) ] [ text <| "Glider" ]
                , button [ onClick (ChangePattern DieHard) ] [ text <| "DieHard" ]
                , button [ onClick (ChangePattern RPentomino) ] [ text <| "The R-pentomino" ]
                , button [ onClick (ChangePattern Acorn) ] [ text <| "Acorn" ]
                , button [ onClick (ChangePattern Talker) ] [ text <| "Talker" ]
                , button [ onClick (ChangePattern GosperGliderGun) ] [ text <| "Gosper Glider Gun" ]
                , button [ onClick Reset ] [ text <| "Reset" ]
                , button [ onClick (ChangeWidth 2) ] [ text <| "Increase Width" ]
                , button [ onClick (ChangeWidth -2) ] [ text <| "Decrease Width" ]
                , button [ onClick (ChangeHeight 2) ] [ text <| "Increase Height" ]
                , button [ onClick (ChangeHeight -2) ] [ text <| "Decrease Height" ]
                ]
            ]


drawGrid : Int -> Int -> Float -> Dict Coordinates BoxStatus -> Mode -> Html Msg
drawGrid height width cellSize boxes mode =
    let
        dimensions =
            { width = width * Basics.round cellSize
            , height = height * Basics.round cellSize
            }

        cellStyle =
            { cellWidth = cellSize
            , cellHeight = cellSize
            , toColor = toColor
            , gridLineWidth = 1
            , gridLineColor = Color.black
            }

        cellGrid =
            CG.initialize { rows = height, columns = width } (getBoxStatus boxes)
    in
    map CellGridMsg <|
        fromUnstyled <|
            CGR.asHtml
                dimensions
                cellStyle
                cellGrid



---- PROGRAM ----


main : Program Int Model Msg
main =
    Browser.element
        { view = view
        , init = init
        , update = update
        , subscriptions = subscriptions
        }


subscriptions : Model -> Sub Msg
subscriptions { mode, speed } =
    case mode of
        Init ->
            Sub.none

        Pause ->
            Sub.none

        Play ->
            Time.every (2000 / (Basics.toFloat <| getSpeed speed)) Tick



---- HELPERS ----


toColor : BoxStatus -> Color.Color
toColor box =
    case box of
        Occupied ->
            Color.green

        _ ->
            Color.red


toggleStatus : BoxStatus -> BoxStatus
toggleStatus b =
    case b of
        Occupied ->
            UnOccupied

        UnOccupied ->
            Occupied


getBoxColor : Dict Coordinates BoxStatus -> Int -> Int -> Color.Color
getBoxColor boxes i j =
    let
        foundBox =
            Dict.get ( i, j ) boxes
    in
    case foundBox of
        Just status ->
            toColor status

        Nothing ->
            toColor UnOccupied


getBoxStatus : Dict Coordinates BoxStatus -> Int -> Int -> BoxStatus
getBoxStatus boxes i j =
    let
        foundBox =
            Dict.get ( i, j ) boxes
    in
    case foundBox of
        Just status ->
            status

        Nothing ->
            UnOccupied


getModeButtonText : Mode -> String
getModeButtonText m =
    case m of
        Init ->
            "Start"

        Play ->
            "Playing"

        Pause ->
            "Paused"


updateCell : Coordinates -> Dict Coordinates BoxStatus -> Dict Coordinates BoxStatus
updateCell coords dict =
    let
        updateFunc : Maybe BoxStatus -> Maybe BoxStatus
        updateFunc currentStatus =
            case currentStatus of
                Nothing ->
                    Just Occupied

                Just _ ->
                    Nothing
    in
    Dict.update coords updateFunc dict


getNewBoxDict : Dict Coordinates BoxStatus -> Dict Coordinates BoxStatus -> Dict Coordinates BoxStatus
getNewBoxDict occupied dict =
    Dict.foldr
        (\k v acc ->
            case getNewStatus2 v << getCount k <| occupied of
                Occupied ->
                    Dict.insert k Occupied acc

                _ ->
                    acc
        )
        Dict.empty
        dict


getCount : Coordinates -> Dict Coordinates BoxStatus -> Int
getCount coords =
    Dict.foldr
        (\ok ov acc ->
            if isNeighbour coords ok then
                acc + 1

            else
                acc
        )
        0


applyGameOfLifeRules : Dict Coordinates BoxStatus -> Dict Coordinates BoxStatus
applyGameOfLifeRules boxes =
    boxes
        -- |> Debug.log "boxes"
        -- |> getNeighbourDict
        -- |> getCountOfOccupiedNeighbours boxes
        -- |> getNewBoxes
        |> getNeighbourDict
        |> getNewBoxDict boxes


getNewBoxes : Dict Coordinates ( BoxStatus, Int ) -> Dict Coordinates BoxStatus
getNewBoxes =
    Dict.foldr
        (\k v acc ->
            let
                newStatus =
                    getNewStatus v
            in
            case newStatus of
                Occupied ->
                    Dict.insert k newStatus acc

                _ ->
                    acc
        )
        Dict.empty


getCountOfOccupiedNeighbours : Dict Coordinates BoxStatus -> Dict Coordinates BoxStatus -> Dict Coordinates ( BoxStatus, Int )
getCountOfOccupiedNeighbours occupied dict =
    let
        countOccupiedNeighbours input =
            Dict.foldr
                (\k v acc ->
                    if isNeighbour input k then
                        acc + 1

                    else
                        acc
                )
                0
                occupied
    in
    Dict.map (\k v -> ( v, countOccupiedNeighbours k )) dict


isNeighbour : Coordinates -> Coordinates -> Bool
isNeighbour ( i, j ) ( m, n ) =
    (i - 1 == m && j - 1 == n)
        || (i - 1 == m && j == n)
        || (i - 1 == m && j + 1 == n)
        || (i == m && j - 1 == n)
        || (i == m && j + 1 == n)
        || (i + 1 == m && j - 1 == n)
        || (i + 1 == m && j == n)
        || (i + 1 == m && j + 1 == n)


getNeighbourDict : Dict Coordinates BoxStatus -> Dict Coordinates BoxStatus
getNeighbourDict occupied =
    Dict.foldr (\k _ acc -> Dict.union acc (getNeighbours k)) occupied occupied


getNeighbourCoords : Coordinates -> List Coordinates
getNeighbourCoords ( r, c ) =
    [ ( r - 1, c - 1 )
    , ( r - 1, c )
    , ( r - 1, c + 1 )
    , ( r, c - 1 )
    , ( r, c + 1 )
    , ( r + 1, c - 1 )
    , ( r + 1, c )
    , ( r + 1, c + 1 )
    ]


getNeighbours : Coordinates -> Dict Coordinates BoxStatus
getNeighbours coords =
    coords
        |> getNeighbourCoords
        |> List.map (\n -> ( n, UnOccupied ))
        |> Dict.fromList



-- get new status of a box based on the count of its neighbours


getNewStatus : ( BoxStatus, Int ) -> BoxStatus
getNewStatus ( prevStatus, n ) =
    case prevStatus of
        Occupied ->
            if n == 2 || n == 3 then
                Occupied

            else
                UnOccupied

        UnOccupied ->
            if n == 3 then
                Occupied

            else
                UnOccupied


getNewStatus2 : BoxStatus -> Int -> BoxStatus
getNewStatus2 prevStatus n =
    case prevStatus of
        Occupied ->
            if n == 2 || n == 3 then
                Occupied

            else
                UnOccupied

        UnOccupied ->
            if n == 3 then
                Occupied

            else
                UnOccupied
