module Main exposing (..)

import Browser exposing (Document)
import CellGrid as CG
import CellGrid.Render as CGR
import Color
import Css exposing (..)
import Dict exposing (Dict)
import Html
import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (class)
import Html.Styled.Events exposing (onClick)
import List.Extra exposing (andThen)
import Styles exposing (boxStyles)
import Task
import Time


type BoxStatus
    = Occupied
    | UnOccupied


type alias Coordinates =
    ( Int, Int )



-- type Box
--     = Box Coordinates BoxStatus
---- MODEL ----


type alias Model =
    { size : Int
    , boxes : Dict Coordinates BoxStatus
    }


init : () -> ( Model, Cmd Msg )
init () =
    ( { size = 4
      , boxes = Dict.fromList <| initBoxes 4
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


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        CellGridMsg _ ->
            ( model, Cmd.none )

        Tick _ ->
            ( { model | boxes = Dict.map (\k v -> toggleStatus v) model.boxes }, Cmd.none )



---- VIEW ----


view : Model -> Html.Html Msg
view model =
    toUnstyled <|
        div []
            [ map CellGridMsg <|
                fromUnstyled <|
                    CGR.asHtml
                        { width = 400, height = 400 }
                        { cellWidth = 100.0, cellHeight = 100.0, toColor = toColor, gridLineWidth = 1, gridLineColor = Color.black }
                        (CG.initialize { rows = 4, columns = 4 } (getBoxStatus model.boxes))
            ]


toColor : BoxStatus -> Color.Color
toColor box =
    case box of
        Occupied ->
            Color.red

        _ ->
            Color.green



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
    Time.every 1000 Tick



---- HELPERS ----


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
