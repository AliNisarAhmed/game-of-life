module Main exposing (..)

import Animator exposing (Animator)
import Animator.Css as ACss
import Browser
import CellGrid as CG
import CellGrid.Render as CGR
import Color
import Element as E exposing (Attribute, Element)
import Element.Input as Input
import Html exposing (Html, map)
import Html.Attributes as Attr
import Icons exposing (..)
import List.Extra as ListExtra
import Patterns
    exposing
        ( Board
        , Coordinates
        , Pattern(..)
        , defaultPattern
        , defaultPatternFunction
        , getPattern
        , oscillator
        , patternKeys
        , patternToString
        , patterns
        )
import Rules exposing (..)
import Styles exposing (black, bookIconStyles, bookStyles, container, explain, gray, gridContainer, gridLayout, gridStyles, heading, hiddenIcon, iconStyles, layout, occupiedColor, paragraph, patternDisplayStyles, primaryColor, ruleElementStyles, ruleRowStyles, settingsIconStyles, settingsStyles, sidebarColumnStyles, sidebarIconStyles, sidebarRowStyles, sidebarStyles, speedControlStyles, subHeading, textStyles, uiStyles, unOccupiedColor, white)
import Time exposing (Posix)


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


type SettingsStatus
    = OpenSettings
    | ClosedSettings


type InitialPattern
    = BuiltInPattern Pattern
    | Custom Board



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
    , pattern : InitialPattern
    , board : Board
    , mode : Mode
    , speed : Speed
    , bookStatus : Animator.Timeline BookStatus
    , generations : Int
    , images : List Image
    , rule : Rule
    , settingsStatus : SettingsStatus
    }


type alias Image =
    { name : String, file : String }


type alias Flags =
    { initialWidth : Int, images : List Image }


init : Flags -> ( Model, Cmd Msg )
init { initialWidth, images } =
    let
        initialHeight =
            70

        initialBoard =
            defaultPatternFunction initialWidth initialHeight
    in
    ( { width = initialWidth
      , height = initialHeight
      , cellSize = 10.0
      , pattern = BuiltInPattern defaultPattern
      , board = initialBoard
      , mode = Init
      , speed = Normal
      , bookStatus = Animator.init Closed
      , generations = 0
      , images = images
      , rule = defaultRule
      , settingsStatus = ClosedSettings
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
    | ForwardFiveSteps
    | ToggleSettings
    | ChangeRule RuleLabels


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
                ( { model | board = updatedDict, pattern = Custom updatedDict }, Cmd.none )

            else
                ( model, Cmd.none )

        Tick _ ->
            ( { model
                | board = applyGameOfLifeRules model.rule model.board
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
                | pattern = BuiltInPattern ptr
                , board = newBoard
                , bookStatus = Animator.go Animator.quickly Closed model.bookStatus
              }
            , Cmd.none
            )

        Reset ->
            let
                newBoard =
                    case model.pattern of
                        BuiltInPattern ptr ->
                            getPattern ptr model.width model.height

                        Custom customPtr ->
                            customPtr
            in
            ( { model
                | mode = Init
                , board = newBoard
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

        ForwardFiveSteps ->
            ( { model | generations = model.generations + 5, board = applyRulesFiveTimes model.rule model.board }, Cmd.none )

        ToggleSettings ->
            let
                newSettingsStatus =
                    toggleSettingsStatus model.settingsStatus
            in
            ( { model | settingsStatus = newSettingsStatus }, Cmd.none )

        ChangeRule label ->
            let
                newRule =
                    Maybe.withDefault defaultRule <| getRule label
            in
            ( { model | rule = newRule, settingsStatus = ClosedSettings }, Cmd.none )



---- VIEW ----


view : Model -> Html Msg
view { height, width, cellSize, mode, board, speed, bookStatus, pattern, generations, images, settingsStatus } =
    let
        currentBookStatus =
            Animator.current bookStatus

        book =
            E.inFront <| displayBook bookStatus images

        settings =
            E.inFront <| displaySettings settingsStatus

        gridContainerStyles =
            gridContainer ++ [ book ] ++ [ settings ]

        content =
            E.column gridContainerStyles <|
                [ E.column uiStyles
                    [ E.row patternDisplayStyles <|
                        [ E.text <| ("Current Pattern: " ++ getPatternName pattern) ]
                    , E.row gridLayout <|
                        [ E.el gridStyles <| drawGrid height width cellSize board mode ]
                    , E.row [ E.width E.fill ] <|
                        [ speedControl speed (Animator.current bookStatus)
                        , displayGeneration generations
                        , displayTimeTravelControls generations mode
                        ]
                    ]
                ]
    in
    E.layout [] <|
        E.el container <|
            E.row layout
                [ sidebar mode speed currentBookStatus
                , content
                ]


drawGrid : Int -> Int -> Float -> Board -> Mode -> Element Msg
drawGrid height width cellSize board mode =
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
            , gridLineColor = gray
            }

        cellGrid =
            CG.initialize { rows = height, columns = width } (getBoxStatus board)
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

        toggleSettingsButton =
            Input.button [] { onPress = Just ToggleSettings, label = settingsIcon }

        playAndResetButtonStyles =
            [ E.centerX ]
    in
    E.column sidebarStyles
        [ E.row bookIconStyles <| [ toggleBookStatusButton ]
        , E.row settingsIconStyles <| [ toggleSettingsButton ]
        , E.row sidebarRowStyles <|
            [ E.column sidebarColumnStyles <|
                [ Input.button playAndResetButtonStyles
                    { onPress = Just <| Reset
                    , label = resetIcon
                    }
                , Input.button playAndResetButtonStyles
                    { onPress = Just <| ChangeMode mode
                    , label = getModeButtonIcon mode
                    }
                ]
            ]
        ]


speedControl : Speed -> BookStatus -> Element Msg
speedControl speed bookStatus =
    E.row speedControlStyles <|
        [ Input.button (speedControlButtonStyles bookStatus)
            { onPress = Just IncreaseSpeed
            , label = increaseSpeedIcon
            }
        , E.el (textStyles ++ speedControlButtonStyles bookStatus ++ [ E.paddingXY 10 0 ]) <| E.text <| speedToString speed
        , Input.button (speedControlButtonStyles bookStatus)
            { onPress = Just DecreaseSpeed
            , label = decreaseSpeedIcon
            }
        ]


speedControlButtonStyles : BookStatus -> List (Attribute Msg)
speedControlButtonStyles bookStatus =
    let
        commonStyles =
            [ E.alignLeft ]
    in
    case bookStatus of
        Open ->
            hiddenIcon ++ commonStyles

        Closed ->
            sidebarIconStyles ++ commonStyles


displayBook : Animator.Timeline BookStatus -> List Image -> Element Msg
displayBook bs images =
    bookContainer bs images


bookContainer : Animator.Timeline BookStatus -> List Image -> Element Msg
bookContainer bs images =
    let
        bookElement =
            openBook bs images

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


openBook : Animator.Timeline BookStatus -> List Image -> Html Msg
openBook bs images =
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
            E.wrappedRow [ E.spacingXY 50 20, E.centerY ] <|
                patternInfoBoxes images
        ]


patternInfoBoxes : List Image -> List (Element Msg)
patternInfoBoxes images =
    List.map
        (\pattern ->
            E.column [ E.spacingXY 10 5 ]
                [ Input.button [ E.width <| E.px 200, E.height <| E.px 200 ]
                    { onPress = Just (ChangePattern pattern)
                    , label =
                        E.image [ E.width <| E.px 200, E.height <| E.px 200 ]
                            { src = Maybe.withDefault placeholderImage <| findPatternImageFile images pattern
                            , description = "Pattern Image"
                            }
                    }
                , E.text <| patternToString pattern
                ]
        )
        patternKeys


findPatternImageFile : List Image -> Pattern -> Maybe String
findPatternImageFile images ptr =
    let
        patternName =
            patternToString ptr

        maybeImage =
            Maybe.map .file <| ListExtra.find (\{ name } -> name == patternName) images
    in
    maybeImage


bookIcon : BookStatus -> Element Msg
bookIcon bs =
    case bs of
        Closed ->
            menuIcon

        Open ->
            crossIcon


getModeButtonIcon : Mode -> Element Msg
getModeButtonIcon mode =
    case mode of
        Init ->
            playIcon

        Play ->
            pauseIcon

        Pause ->
            playIcon


displayGeneration : Int -> Element Msg
displayGeneration generations =
    let
        styles =
            textStyles ++ [ E.centerX, E.width <| E.fillPortion 2 ]
    in
    if generations == 0 then
        E.row styles [ E.text "" ]

    else
        E.row styles [ E.text <| "Generations: " ++ String.fromInt generations ]


displayTimeTravelControls : Int -> Mode -> Element Msg
displayTimeTravelControls generations mode =
    let
        styles =
            [ E.width <| E.fillPortion 1, E.alignRight ]
    in
    if generations == 0 || mode /= Pause then
        E.row styles <| [ E.text "" ]

    else
        E.row styles
            [ Input.button [ E.htmlAttribute (Attr.title "Forward 1 step"), E.alignRight ]
                { onPress = Just (Tick <| Time.millisToPosix 1), label = travelForwardIcon }
            , Input.button [ E.htmlAttribute (Attr.title "Forward 5 steps"), E.alignRight ]
                { onPress = Just ForwardFiveSteps, label = travelForwardFastIcon }
            ]


displaySettings : SettingsStatus -> Element Msg
displaySettings settingsStatus =
    let
        settingsElement =
            case settingsStatus of
                OpenSettings ->
                    openSettings

                ClosedSettings ->
                    hiddenSettings

        pointerEvents =
            case settingsStatus of
                ClosedSettings ->
                    Attr.style "pointer-events" "none"

                OpenSettings ->
                    Attr.style "" ""
    in
    E.html <|
        Html.div
            [ Attr.style "position" "relative"
            , Attr.style "width" "100%"
            , Attr.style "height" "100%"
            , Attr.style "overflow" "hidden"
            , Attr.class "bookContainer"
            , pointerEvents
            ]
            [ settingsElement ]


openSettings : Html Msg
openSettings =
    Html.div
        [ Attr.style "position" "absolute"
        , Attr.style "left" "0px"
        , Attr.style "top" "0px"
        , Attr.style "right" "70%"
        , Attr.style "bottom" "0px"
        , Attr.style "flex-wrap" "wrap"
        , Attr.style "align-items" "flex-start"
        , Attr.style "justify-content" "space-between"
        , Attr.style "background-color" "gray"
        , Attr.style "white-space" "normal"
        , Attr.class "openBook"
        ]
        [ E.layoutWith { options = [ E.noStaticStyleSheet ] } settingsStyles <|
            E.column [ E.width E.fill, E.spacingXY 0 20 ] <|
                ruleList
        ]


hiddenSettings : Html Msg
hiddenSettings =
    Html.div [] []


ruleList : List (Element Msg)
ruleList =
    List.map
        (\( label, ( rule, ruleDescription ) ) ->
            E.row ruleRowStyles <| [ ruleElement label rule ruleDescription ]
        )
        rulesList


ruleElement : RuleLabels -> Rule -> Description -> Element Msg
ruleElement label (Rule born survive) description =
    Input.button ruleElementStyles
        { onPress = Just (ChangeRule label)
        , label =
            E.column [ E.width E.fill, E.spacingXY 0 20 ]
                [ E.el heading <| E.text <| rulesToString label
                , E.el subHeading
                    (E.text
                        ("B"
                            ++ (String.concat <| List.map (\b -> String.fromInt b) born)
                            ++ "S"
                            ++ (String.concat <| List.map (\s -> String.fromInt s) survive)
                        )
                    )
                , E.el []
                    (E.paragraph paragraph [ E.text description ])
                ]
        }



---- PROGRAM ----


placeholderImage : String
placeholderImage =
    "https://via.placeholder.com/300"


main : Program Flags Model Msg
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


getPatternName : InitialPattern -> String
getPatternName ip =
    case ip of
        BuiltInPattern ptr ->
            patternToString ptr

        Custom _ ->
            "Custom"


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


getBoxStatus : Board -> Int -> Int -> BoxStatus
getBoxStatus board i j =
    if List.member ( i, j ) board then
        Occupied

    else
        UnOccupied


updateCell : Coordinates -> Board -> Board
updateCell coords currentlyAlive =
    if List.member coords currentlyAlive then
        List.filter (\c -> c /= coords) currentlyAlive

    else
        coords :: currentlyAlive


toggleSettingsStatus : SettingsStatus -> SettingsStatus
toggleSettingsStatus s =
    case s of
        OpenSettings ->
            ClosedSettings

        ClosedSettings ->
            OpenSettings



---- Game of Life Algorithm ----


isAlive : Board -> Coordinates -> Bool
isAlive board coord =
    List.member coord board


isDead : Board -> Coordinates -> Bool
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


countLiveNeighbours : Board -> Coordinates -> Int
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


survivors : Born -> Board -> Board
survivors bornRules board =
    List.foldl
        (\x acc ->
            if List.member (countLiveNeighbours board x) bornRules then
                x :: acc

            else
                acc
        )
        []
        board


births : Born -> Board -> Board
births born board =
    List.foldl
        (\x result ->
            let
                validNeighbs =
                    List.foldr
                        (\f acc ->
                            let
                                neighbour =
                                    f x
                            in
                            if
                                (not <| List.member neighbour board)
                                    && (not <| List.member neighbour result)
                                    && List.member (countLiveNeighbours board neighbour) born
                            then
                                neighbour :: acc

                            else
                                acc
                        )
                        []
                        neighFunctions
            in
            validNeighbs ++ result
        )
        []
        board


removeDuplicates : Board -> Board
removeDuplicates board =
    case board of
        [] ->
            []

        x :: xs ->
            x :: (removeDuplicates <| List.filter (\c -> c /= x) xs)


applyGameOfLifeRules : Rule -> Board -> Board
applyGameOfLifeRules (Rule birthRules surviveRules) board =
    births birthRules board ++ survivors surviveRules board


applyRulesFiveTimes : Rule -> Board -> Board
applyRulesFiveTimes rule =
    applyGameOfLifeRules rule
        >> applyGameOfLifeRules rule
        >> applyGameOfLifeRules rule
        >> applyGameOfLifeRules rule
        >> applyGameOfLifeRules rule


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


getNeighbourCoords : Coordinates -> Board
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
