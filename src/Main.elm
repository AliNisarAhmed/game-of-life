module Main exposing (..)

import Animator as Animator
import Animator.Inline as Inline
import Browser exposing (Document)
import Color
import Element as E exposing (Attribute, Element)
import Element.Background as Background
import Element.Border as Border
import Element.Events as Event
import Html exposing (Html)
import Task
import Time as Time



---- MODEL ----


type alias Model =
    { status : Animator.Timeline Status, time : Time.Posix }


type Status
    = On
    | Off


init : ( Model, Cmd Msg )
init =
    ( { status = Animator.init Off, time = Time.millisToPosix 0 }, Task.perform NewTime Time.now )


animator : Animator.Animator Model
animator =
    Animator.animator
        |> Animator.watching .status (\newStatus model -> { model | status = newStatus })


animationSub : Model -> Sub Msg
animationSub model =
    animator |> Animator.toSubscription Tick model



---- UPDATE ----


type Msg
    = NoOp
    | Tick Time.Posix
    | Clicked Status
    | NewTime Time.Posix


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        NewTime currentTime ->
            ( { model | time = currentTime }, Cmd.none )

        Tick newTime ->
            ( Animator.update newTime animator model, Cmd.none )

        Clicked newStatus ->
            ( { model | status = model.status |> Animator.go Animator.quickly newStatus }, Cmd.none )



---- VIEW ----


view : Model -> Html Msg
view model =
    E.layout [ E.width E.fill, E.height E.fill, Background.color <| E.rgb255 150 150 150 ] <|
        E.el
            [ E.centerX
            , E.centerY
            , E.width <| E.px 40
            , E.height <| E.px 40
            , Border.width 1
            , Border.solid
            , Border.color <| E.rgb255 0 0 0
            , Event.onClick <| Clicked (invertStatus <| Animator.current model.status)
            , E.htmlAttribute <|
                Inline.backgroundColor model.status <|
                    \state ->
                        if state == On then
                            Color.blue

                        else
                            Color.white
            ]
            E.none



---- PROGRAM ----


main : Program () Model Msg
main =
    Browser.element
        { view = view
        , init = \_ -> init
        , update = update
        , subscriptions = animationSub
        }



---- HELPERS ----


invertStatus : Status -> Status
invertStatus s =
    case s of
        On ->
            Off

        Off ->
            On
