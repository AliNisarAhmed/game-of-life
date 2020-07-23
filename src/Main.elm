module Main exposing (..)

import Browser exposing (Document)
import Css exposing (..)
import Html
import Html.Styled exposing (..)
import Html.Styled.Events exposing (onClick)
import Styles exposing (boxStyles)
import Task



---- MODEL ----


type alias Model =
    { clicked : Bool }


init : ( Model, Cmd Msg )
init =
    ( { clicked = False }, Cmd.none )



---- UPDATE ----


type Msg
    = NoOp
    | ButtonClicked


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        ButtonClicked ->
            ( { model | clicked = not model.clicked }, Cmd.none )



---- VIEW ----


view : Model -> Html.Html Msg
view model =
    toUnstyled <|
        div []
            [ box model.clicked ]


box : Bool -> Html Msg
box clicked =
    let
        boxAttr =
            [ onClick ButtonClicked, boxStyles clicked ]
    in
    div boxAttr []



---- PROGRAM ----


main : Program () Model Msg
main =
    Browser.element
        { view = view
        , init = \_ -> init
        , update = update
        , subscriptions = always Sub.none
        }
