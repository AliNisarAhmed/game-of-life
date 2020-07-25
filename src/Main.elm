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


init : () -> ( Model, Cmd Msg )
init () =
    ( { size = 4
      , boxes = Dict.fromList <| initBoxes 4
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
        |> andThen (\v1 -> List.map (\v2 -> ( ( v1, v2 ), Occupied )) values)



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
            ( { model | boxes = Dict.map (\k v -> toggleStatus v) model.boxes }, Cmd.none )

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
    toUnstyled <|
        div [ container ]
            [ map CellGridMsg <|
                fromUnstyled <|
                    CGR.asHtml
                        { width = 400, height = 400 }
                        { cellWidth = 100.0, cellHeight = 100.0, toColor = toColor, gridLineWidth = 1, gridLineColor = Color.black }
                        (CG.initialize { rows = 4, columns = 4 } (getBoxStatus model.boxes))
            , button [ onClick (ChangeMode model.mode) ] [ text <| getModeButtonText model.mode ]
            ]



---- PROGRAM ----


main : Program () Model Msg
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
        updateFunc =
            Maybe.map toggleStatus
    in
    Dict.update coords updateFunc dict
