module Main exposing (..)

import Animator exposing (Animator)
import Animator.Css as ACss
import Browser
import CellGrid as CG
import CellGrid.Render as CGR
import Color
import Css exposing (valid)
import Element as E exposing (Attribute, Element)
import Element.Input as Input
import FeatherIcons
import Html exposing (Html)
import Html.Attributes as Attr
import Patterns
    exposing
        ( Board
        , Coordinates
        , Pattern(..)
        , defaultPattern
        , defaultPatternFunction
        , getPattern
        , getPatternName
        , oscillator
        , patternKeys
        , patternToString
        , patterns
        )
import Styles exposing (black, bookStyles, container, explain, gridContainer, gridLayout, gridStyles, hiddenIcon, iconStyles, layout, occupiedColor, patternDisplayStyles, sidebarColumnStyles, sidebarIconStyles, sidebarRowStyles, sidebarStyles, textStyles, unOccupiedColor)
import Time


type BoxStatus
    = Occupied
    | UnOccupied


type Mode
    = Init
    | Play
    | Pause


type BookStatus
    = Open
    | Closed


type Rule
    = Rule Born Survive


type alias Born =
    List Int


type alias Survive =
    List Int



---- SPEED ----


type Speed
    = Slow
    | Normal
    | Fast


increaseSpeed : Speed -> Speed
increaseSpeed spd =
    case spd of
        Slow ->
            Normal

        Normal ->
            Fast

        Fast ->
            Fast


decreaseSpeed : Speed -> Speed
decreaseSpeed spd =
    case spd of
        Fast ->
            Normal

        Normal ->
            Slow

        Slow ->
            Slow


speedToString : Speed -> String
speedToString spd =
    case spd of
        Slow ->
            "Slow"

        Normal ->
            "Normal"

        Fast ->
            "Fast"


speedToValue : Speed -> Int
speedToValue speed =
    case speed of
        Slow ->
            10

        Normal ->
            20

        Fast ->
            30



---- MODEL ----


type alias Model =
    { height : Int
    , width : Int
    , cellSize : Float
    , pattern : Maybe Pattern
    , board : Board
    , mode : Mode
    , speed : Speed
    , bookStatus : Animator.Timeline BookStatus
    , generations : Int
    }


init : Int -> ( Model, Cmd Msg )
init initialWidth =
    let
        initialHeight =
            70

        initialBoard =
            defaultPatternFunction initialWidth initialHeight
    in
    ( { width = initialWidth
      , height = initialHeight
      , cellSize = 10.0
      , pattern = Just defaultPattern
      , board = initialBoard
      , mode = Init
      , speed = Normal
      , bookStatus = Animator.init Closed
      , generations = 0
      }
    , Cmd.none
    )


animator : Animator.Animator Model
animator =
    Animator.animator
        |> ACss.watching
            .bookStatus
            (\newBookStatus model -> { model | bookStatus = newBookStatus })



---- UPDATE ----


type Msg
    = NoOp
    | CellGridMsg CGR.Msg
    | Tick Time.Posix
    | ChangeMode Mode
    | IncreaseSpeed
    | DecreaseSpeed
    | ChangePattern Pattern
    | ChangeWidth Int
    | ChangeHeight Int
    | ToggleBookStatus
    | Reset
    | AnimatorSubscriptionMsg Time.Posix


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
                        updateCell coordinates model.board
                in
                ( { model | board = updatedDict, pattern = Nothing }, Cmd.none )

            else
                ( model, Cmd.none )

        Tick _ ->
            ( { model
                | board = applyGameOfLifeRules model.board
                , generations = model.generations + 1
              }
            , Cmd.none
            )

        IncreaseSpeed ->
            ( { model | speed = increaseSpeed model.speed }, Cmd.none )

        DecreaseSpeed ->
            ( { model | speed = decreaseSpeed model.speed }, Cmd.none )

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
            let
                newBoard =
                    getPattern ptr model.width model.height
            in
            ( { model
                | pattern = Just ptr
                , board = newBoard
                , bookStatus = Animator.go Animator.quickly Closed model.bookStatus
              }
            , Cmd.none
            )

        Reset ->
            ( { model
                | mode = Init

                -- TODO: Store user's custom board so that when they reset when in custom, we display their custom board
                , board = getPattern (Maybe.withDefault defaultPattern model.pattern) model.width model.height
                , generations = 0
              }
            , Cmd.none
            )

        ChangeWidth n ->
            ( { model | width = model.width + n }, Cmd.none )

        ChangeHeight n ->
            ( { model | height = model.height + n }, Cmd.none )

        ToggleBookStatus ->
            let
                newBookStatus =
                    toggleBookStatus <| Animator.current model.bookStatus
            in
            ( { model | bookStatus = Animator.go Animator.quickly newBookStatus model.bookStatus }, Cmd.none )

        AnimatorSubscriptionMsg newTime ->
            ( Animator.update newTime animator model, Cmd.none )



---- VIEW ----


view : Model -> Html Msg
view { height, width, cellSize, mode, board, speed, bookStatus, pattern, generations } =
    let
        currentBookStatus =
            Animator.current bookStatus

        book =
            case currentBookStatus of
                Open ->
                    E.inFront <| displayBook bookStatus

                Closed ->
                    E.inFront <| displayBook bookStatus

        -- E.inFront <| displayBook bookStatus
        gridContainerStyles =
            gridContainer ++ [ book ]

        uiStyles =
            [ E.centerX, E.centerY, E.spacingXY 0 10 ]

        content =
            E.column gridContainerStyles <|
                [ E.column uiStyles
                    [ E.row patternDisplayStyles <|
                        [ E.text <| ("Current Pattern: " ++ getPatternName pattern) ]
                    , E.row gridLayout <|
                        [ E.el gridStyles <| drawGrid height width cellSize board mode ]
                    , E.row [] <|
                        [ displayGeneration generations ]
                    ]
                ]
    in
    E.layout [] <|
        E.el container <|
            E.row layout
                [ sidebar mode speed currentBookStatus
                , content
                ]


drawGrid : Int -> Int -> Float -> List Coordinates -> Mode -> Element Msg
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
            , gridLineColor = black
            }

        cellGrid =
            CG.initialize { rows = height, columns = width } (getBoxStatus boxes)
    in
    E.html <|
        Html.map CellGridMsg <|
            CGR.asHtml
                dimensions
                cellStyle
                cellGrid


sidebar : Mode -> Speed -> BookStatus -> Element Msg
sidebar mode speed bookStatus =
    let
        toggleBookStatusButton =
            Input.button [] { onPress = Just ToggleBookStatus, label = bookIcon bookStatus }
    in
    E.column sidebarStyles
        [ E.row sidebarRowStyles <|
            [ E.column sidebarColumnStyles
                [ Input.button (sidebarButtonStyles bookStatus)
                    { onPress = Just IncreaseSpeed
                    , label = increaseSpeedIcon
                    }
                , E.el (textStyles ++ sidebarButtonStyles bookStatus) <| E.text <| speedToString speed
                , Input.button (sidebarButtonStyles bookStatus)
                    { onPress = Just DecreaseSpeed
                    , label = decreaseSpeedIcon
                    }
                ]
            ]
        , E.row sidebarRowStyles <| [ toggleBookStatusButton ]
        , E.row sidebarRowStyles <|
            [ E.column sidebarColumnStyles <|
                [ Input.button (sidebarButtonStyles bookStatus)
                    { onPress = Just <| Reset
                    , label = resetIcon
                    }
                , Input.button (sidebarButtonStyles bookStatus)
                    { onPress = Just <| ChangeMode mode
                    , label = getModeButtonIcon mode
                    }
                ]
            ]
        ]


sidebarButtonStyles : BookStatus -> List (Attribute Msg)
sidebarButtonStyles bookStatus =
    case bookStatus of
        Open ->
            hiddenIcon

        Closed ->
            sidebarIconStyles


decreaseSpeedIcon : Element Msg
decreaseSpeedIcon =
    FeatherIcons.chevronsDown
        |> FeatherIcons.withClass "icon"
        |> FeatherIcons.toHtml iconStyles
        |> E.html


increaseSpeedIcon : Element Msg
increaseSpeedIcon =
    FeatherIcons.chevronsUp
        |> FeatherIcons.withClass "icon"
        |> FeatherIcons.toHtml iconStyles
        |> E.html


displayBook : Animator.Timeline BookStatus -> Element Msg
displayBook bs =
    bookContainer bs


bookContainer : Animator.Timeline BookStatus -> Element Msg
bookContainer bs =
    let
        bookElement =
            case Animator.current bs of
                Closed ->
                    openBook bs

                Open ->
                    openBook bs

        pointerEvents =
            case Animator.current bs of
                Closed ->
                    Attr.style "pointer-events" "none"

                _ ->
                    Attr.style "" ""
    in
    E.html <|
        ACss.div bs
            []
            [ Attr.style "position" "relative"
            , Attr.style "width" "100%"
            , Attr.style "height" "100%"
            , Attr.style "overflow" "hidden"
            , pointerEvents
            , Attr.class "bookContainer"
            ]
            [ bookElement ]


hiddenBook : Animator.Timeline BookStatus -> Html Msg
hiddenBook bs =
    Html.div
        [ Attr.style "display" "none"
        , Attr.class "hiddenBook"
        ]
        []


openBook : Animator.Timeline BookStatus -> Html Msg
openBook bs =
    ACss.div bs
        [ ACss.opacity <|
            \state ->
                case state of
                    Open ->
                        Animator.at 1

                    Closed ->
                        Animator.at 0
        , ACss.style "transform"
            (\f ->
                if f == 0 then
                    "translateX(100%);"

                else if f == 1 then
                    "translateX(0%);"

                else
                    ""
            )
            (\state ->
                case state of
                    Open ->
                        Animator.at 1

                    Closed ->
                        Animator.at 0
            )
        ]
        [ Attr.style "position" "absolute"
        , Attr.style "left" "0px"
        , Attr.style "top" "0px"
        , Attr.style "right" "0px"
        , Attr.style "bottom" "0px"
        , Attr.style "flex-wrap" "wrap"
        , Attr.style "align-items" "flex-start"
        , Attr.style "justify-content" "space-between"
        , Attr.style "background-color" "gray"
        , Attr.style "white-space" "normal"
        , Attr.class "openBook"
        ]
        [ E.layoutWith { options = [ E.noStaticStyleSheet ] } bookStyles <|
            E.wrappedRow [ E.spacingXY 50 20, E.centerY ] patternInfoBoxes
        ]


patternInfoBoxes : List (Element Msg)
patternInfoBoxes =
    List.map
        (\pattern ->
            E.column [ E.spacingXY 10 5 ]
                [ Input.button []
                    { onPress = Just (ChangePattern pattern)
                    , label =
                        E.image []
                            { src = placeholderImage, description = "Pattern Image" }
                    }
                , E.text <| patternToString pattern
                ]
        )
        patternKeys


bookIcon : BookStatus -> Element Msg
bookIcon bs =
    case bs of
        Closed ->
            FeatherIcons.menu
                |> FeatherIcons.withClass "icon"
                |> FeatherIcons.toHtml iconStyles
                |> E.html

        Open ->
            FeatherIcons.x
                |> FeatherIcons.withClass "icon"
                |> FeatherIcons.toHtml iconStyles
                |> E.html


resetIcon : Element Msg
resetIcon =
    FeatherIcons.refreshCw
        |> FeatherIcons.withClass "icon"
        |> FeatherIcons.toHtml iconStyles
        |> E.html


getModeButtonIcon : Mode -> Element Msg
getModeButtonIcon mode =
    case mode of
        Init ->
            FeatherIcons.play
                |> FeatherIcons.withSize 40
                |> FeatherIcons.withClass "icon"
                |> FeatherIcons.toHtml iconStyles
                |> E.html

        Play ->
            FeatherIcons.pause
                |> FeatherIcons.withSize 40
                |> FeatherIcons.withClass "icon"
                |> FeatherIcons.toHtml iconStyles
                |> E.html

        Pause ->
            FeatherIcons.play
                |> FeatherIcons.withSize 40
                |> FeatherIcons.withClass "icon"
                |> FeatherIcons.toHtml iconStyles
                |> E.html



---- PROGRAM ----


displayGeneration : Int -> Element Msg
displayGeneration generations =
    if generations == 0 then
        E.row textStyles <| [ E.text "" ]

    else
        E.row textStyles [ E.text <| "Generations: " ++ String.fromInt generations ]


placeholderImage : String
placeholderImage =
    "https://via.placeholder.com/300"


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
            Animator.toSubscription AnimatorSubscriptionMsg model animator

        Pause ->
            Animator.toSubscription AnimatorSubscriptionMsg model animator

        Play ->
            Time.every (2000 / (Basics.toFloat <| speedToValue model.speed)) Tick



---- HELPERS ----


toColor : BoxStatus -> Color.Color
toColor box =
    case box of
        Occupied ->
            occupiedColor

        _ ->
            unOccupiedColor


toggleBookStatus : BookStatus -> BookStatus
toggleBookStatus bs =
    case bs of
        Open ->
            Closed

        Closed ->
            Open


getBoxStatus : List Coordinates -> Int -> Int -> BoxStatus
getBoxStatus boxes i j =
    if List.member ( i, j ) boxes then
        Occupied

    else
        UnOccupied


updateCell : Coordinates -> List Coordinates -> List Coordinates
updateCell coords currentlyAlive =
    if List.member coords currentlyAlive then
        List.filter (\c -> c /= coords) currentlyAlive

    else
        coords :: currentlyAlive



---- Game of Life Algorithm ----


isAlive : List Coordinates -> Coordinates -> Bool
isAlive board coord =
    List.member coord board


isDead : List Coordinates -> Coordinates -> Bool
isDead board coord =
    not <| isAlive board coord


neighFunctions : List (Coordinates -> Coordinates)
neighFunctions =
    [ \( r, c ) -> ( r - 1, c - 1 )
    , \( r, c ) -> ( r - 1, c )
    , \( r, c ) -> ( r - 1, c + 1 )
    , \( r, c ) -> ( r, c - 1 )
    , \( r, c ) -> ( r, c + 1 )
    , \( r, c ) -> ( r + 1, c - 1 )
    , \( r, c ) -> ( r + 1, c )
    , \( r, c ) -> ( r + 1, c + 1 )
    ]


countLiveNeighbours : List Coordinates -> Coordinates -> Int
countLiveNeighbours board coord =
    List.foldl
        (\x acc ->
            if isNeighbour x coord then
                acc + 1

            else
                acc
        )
        0
        board


survivors : List Coordinates -> List Coordinates
survivors board =
    List.foldl
        (\x acc ->
            if List.member (Debug.log "countLive: " <| countLiveNeighbours board x) [ 2, 3 ] then
                x :: acc

            else
                acc
        )
        []
        board


births : List Coordinates -> List Coordinates
births board =
    List.foldl
        (\x result ->
            let
                validNeighbs =
                    List.foldl
                        (\f acc ->
                            let
                                neighbour =
                                    f x
                            in
                            if isDead board neighbour && (not <| List.member neighbour acc) then
                                if countLiveNeighbours board neighbour == 3 then
                                    neighbour :: acc

                                else
                                    acc

                            else
                                acc
                        )
                        []
                        neighFunctions
            in
            validNeighbs
        )
        []
        board


removeDuplicates : List Coordinates -> List Coordinates
removeDuplicates board =
    case board of
        [] ->
            []

        x :: xs ->
            x :: (removeDuplicates <| List.filter (\c -> c /= x) xs)


applyGameOfLifeRules : List Coordinates -> List Coordinates
applyGameOfLifeRules board =
    survivors board ++ births board


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
