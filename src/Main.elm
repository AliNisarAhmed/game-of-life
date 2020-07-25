module Main exposing (..)

import Browser
import CellGrid as CG
import CellGrid.Render as CGR
import Color
import Css exposing (..)
import Dict exposing (Dict)
import Html
import Html.Styled exposing (..)
import Html.Styled.Events exposing (onClick)
import List.Extra exposing (andThen)
import Maybe.Extra exposing (isJust)
import Styles exposing (container)
import Time


type BoxStatus
    = Occupied
    | UnOccupied


type alias Coordinates =
    ( Int, Int )


type Mode
    = Init
    | Play
    | Pause



---- MODEL ----


type alias Model =
    { size : Int
    , boxes : Dict Coordinates BoxStatus
    , mode : Mode
    }


init : Int -> ( Model, Cmd Msg )
init initialSize =
    ( { size = initialSize
      , boxes = Dict.empty
      , mode = Init
      }
    , Cmd.none
    )


initBoxes : Int -> List ( Coordinates, BoxStatus )
initBoxes n =
    let
        values =
            List.range 0 (n - 1)
    in
    values
        |> andThen (\v1 -> List.map (\v2 -> ( ( v1, v2 ), UnOccupied )) values)



---- UPDATE ----


type Msg
    = NoOp
    | CellGridMsg CGR.Msg
    | Tick Time.Posix
    | ChangeMode Mode


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



---- VIEW ----


view : Model -> Html.Html Msg
view model =
    let
        cellSize =
            50
    in
    toUnstyled <|
        div [ container ]
            [ map CellGridMsg <|
                fromUnstyled <|
                    CGR.asHtml
                        { width = model.size * cellSize, height = model.size * cellSize }
                        { cellWidth = cellSize, cellHeight = cellSize, toColor = toColor, gridLineWidth = 1, gridLineColor = Color.black }
                        (CG.initialize { rows = model.size, columns = model.size } (getBoxStatus model.boxes))
            , button [ onClick (ChangeMode model.mode) ] [ text <| getModeButtonText model.mode ]
            ]



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
subscriptions model =
    case model.mode of
        Init ->
            Sub.none

        Pause ->
            Sub.none

        Play ->
            Time.every 1000 Tick



---- HELPERS ----


toColor : BoxStatus -> Color.Color
toColor box =
    case box of
        Occupied ->
            Color.red

        _ ->
            Color.green


toggleStatus : BoxStatus -> BoxStatus
toggleStatus b =
    case b of
        Occupied ->
            UnOccupied

        UnOccupied ->
            Occupied


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


applyGameOfLifeRules : Dict Coordinates BoxStatus -> Dict Coordinates BoxStatus
applyGameOfLifeRules boxes =
    boxes
        |> Debug.log "boxes"
        |> getNeighbourDict
        |> Debug.log "after getNeighbour Dict"
        |> Dict.union boxes
        |> Debug.log "after union with boxes"
        |> getCountOfOccupiedNeighbours boxes
        |> Debug.log "get count of occupied neighbours"
        |> getNewBoxes
        |> filterOccupiedCells


getNewBoxes : Dict Coordinates ( BoxStatus, Int ) -> Dict Coordinates BoxStatus
getNewBoxes =
    Dict.map (\k v -> getNewStatus v)


getCountOfOccupiedNeighbours : Dict Coordinates BoxStatus -> Dict Coordinates BoxStatus -> Dict Coordinates ( BoxStatus, Int )
getCountOfOccupiedNeighbours occupied dict =
    let
        countOccupiedNeighbours k =
            getNeighbourCoords k
                |> List.foldr
                    (\x acc ->
                        if isJust (Dict.get x occupied) then
                            acc + 1

                        else
                            acc
                    )
                    0
    in
    dict
        |> Dict.map (\k v -> ( v, countOccupiedNeighbours k ))


getNeighbourDict : Dict Coordinates BoxStatus -> Dict Coordinates BoxStatus
getNeighbourDict occupied =
    Dict.foldr (\k _ acc -> Dict.union acc (getNeighbours k)) Dict.empty occupied


filterOccupiedCells : Dict comparable BoxStatus -> Dict comparable BoxStatus
filterOccupiedCells =
    Dict.filter (\_ v -> v == Occupied)


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
            if n < 2 then
                UnOccupied

            else if n == 2 || n == 3 then
                Occupied

            else if n > 3 then
                UnOccupied

            else
                UnOccupied

        UnOccupied ->
            if n == 3 then
                Occupied

            else
                UnOccupied
