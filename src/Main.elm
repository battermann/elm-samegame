module Main exposing (Model, Msg(..), init, main, update, view)

import Browser
import Browser.Dom
import Browser.Events
import Element exposing (Element, text)
import Element.Background as Background
import Element.Border as Border
import Element.Events as Events
import Element.Font as Font
import Element.Input as Input
import Html exposing (Html)
import Html.Attributes as Attributes
import Http
import Json.Decode as Decode
import MD5
import SameGame exposing (Block, BlockState(..), Board, Color(..), Column, Game(..), Position)
import Task



---- MODEL ----


data : Int -> String -> String
data score nickname =
    let
        stringToHash =
            "71924738-62fe-4543-9d37-2e666bb27df8" ++ String.fromInt score ++ nickname ++ ""

        hash =
            MD5.hex stringToHash
    in
    "id=" ++ "71924738-62fe-4543-9d37-2e666bb27df8" ++ "&nickname=" ++ nickname ++ "&score=" ++ String.fromInt score ++ "&hash=" ++ hash


type RemoteData e a
    = NotAsked
    | Loading
    | Reloading a
    | Failure e
    | Success a


type Modal
    = HighScores
    | Rules


type alias Window =
    { width : Int
    , height : Int
    }


type alias Model =
    { game : Game
    , selectedBlocks : List Position
    , modal : Maybe Modal
    , topTen : List String
    , playerName : Maybe String
    , highScores : RemoteData String (List HighScoreEntry)
    , window : Window
    }


toMsg : Browser.Dom.Viewport -> Msg
toMsg { viewport } =
    WindowResize (viewport.width |> round) (viewport.height |> round)


init : ( Model, Cmd Msg )
init =
    ( { game = SameGame.newGame 1
      , selectedBlocks = []
      , modal = Nothing
      , topTen = []
      , playerName = Nothing
      , highScores = NotAsked
      , window = Window 0 0
      }
    , Task.perform toMsg Browser.Dom.getViewport
    )


type alias HighScoreEntry =
    { nickname : String
    , score : Int
    }



---- HTTP ----


add : Model -> Cmd Msg
add model =
    Http.post "https://cors-anywhere.herokuapp.com/https://omgleaderboards.appspot.com/add" (Http.stringBody "application/x-www-form-urlencoded" (data (SameGame.score model.game) (model.playerName |> Maybe.withDefault ""))) (Decode.succeed "")
        |> Http.send AddToLeaderBoardResult


decodeHighScoreEntries : Decode.Decoder (List HighScoreEntry)
decodeHighScoreEntries =
    let
        singleEntry =
            Decode.map2 HighScoreEntry (Decode.at [ "nickname" ] Decode.string) (Decode.at [ "score" ] Decode.int)
    in
    Decode.at [ "scores", "alltime" ] (Decode.list singleEntry)


highscores : Cmd Msg
highscores =
    Http.get
        "https://cors-anywhere.herokuapp.com/https://omgleaderboards.appspot.com/get/71924738-62fe-4543-9d37-2e666bb27df8?timeframes=alltime&limit=10"
        decodeHighScoreEntries
        |> Http.send HighScoresResult



---- UPDATE ----


type Msg
    = Click Position
    | SelectGroup Position
    | Dialog (Maybe Modal)
    | NewGame
    | HighScoresResult (Result Http.Error (List HighScoreEntry))
    | NameInputChange String
    | AddToLeaderBoard
    | AddToLeaderBoardResult (Result Http.Error String)
    | WindowResize Int Int


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Click position ->
            ( { model | game = SameGame.play position model.game, selectedBlocks = [] }, Cmd.none )

        SelectGroup position ->
            ( { model | selectedBlocks = SameGame.groupPositions model.game position }, Cmd.none )

        Dialog (Just HighScores) ->
            case model.highScores of
                Success scores ->
                    ( { model | modal = Just HighScores, highScores = Reloading scores }, highscores )

                _ ->
                    ( { model | modal = Just HighScores, highScores = Loading }, highscores )

        Dialog modal ->
            ( { model | modal = modal }, Cmd.none )

        NewGame ->
            init

        HighScoresResult (Err _) ->
            ( { model | highScores = Failure "Http Error" }, Cmd.none )

        HighScoresResult (Ok scores) ->
            ( { model | highScores = Success scores }, Cmd.none )

        NameInputChange name ->
            ( { model | playerName = Just name }, Cmd.none )

        AddToLeaderBoard ->
            init |> Tuple.mapSecond (\cmd -> Cmd.batch [ cmd, add model ])

        AddToLeaderBoardResult _ ->
            ( model, Cmd.none )

        WindowResize width height ->
            ( { model | window = Window width height }, Cmd.none )



---- VIEW ----


viewColor : Float -> Color -> Element.Attribute Msg
viewColor a (Color color) =
    case color of
        0 ->
            Background.color (Element.rgba255 80 80 80 a)

        1 ->
            Background.color (Element.rgba255 133 141 185 a)

        2 ->
            Background.color (Element.rgba255 194 83 59 a)

        3 ->
            Background.color (Element.rgba255 130 203 80 a)

        4 ->
            Background.color (Element.rgba255 142 196 167 a)

        _ ->
            Background.color (Element.rgba255 50 50 50 a)


viewCell : List Position -> Int -> Int -> BlockState -> Element Msg
viewCell selectedBlocks columnIndex rowIndex cellState =
    let
        position =
            { col = columnIndex, row = 14 - rowIndex }
    in
    case cellState of
        Filled color ->
            Element.el
                ([ Element.height Element.fill
                 , Element.width Element.fill
                 , viewColor (alpha selectedBlocks position) color
                 , Events.onClick (Click position)
                 , Events.onMouseEnter (SelectGroup position)
                 ]
                    ++ cursor selectedBlocks position
                )
                Element.none

        Empty ->
            Element.el
                [ Element.height Element.fill
                , Element.width Element.fill
                ]
                Element.none


cursor : List Position -> Position -> List (Element.Attribute Msg)
cursor selectedGroup position =
    if selectedGroup |> List.member position then
        [ Element.htmlAttribute (Attributes.style "cursor" "pointer") ]

    else
        []


alpha : List Position -> Position -> Float
alpha selectedGroup position =
    if selectedGroup |> List.member position then
        0.5

    else
        1.0


viewColumn : List Position -> Int -> Column -> Element Msg
viewColumn selectedBlocks index column =
    column
        |> List.reverse
        |> List.indexedMap (viewCell selectedBlocks index)
        |> Element.column
            [ Element.height Element.fill
            , Element.width Element.fill
            , Element.spacing 1
            ]


boardWidth : Window -> Int
boardWidth window =
    if window.width > window.height then
        round (toFloat window.height * 0.7) |> min 300

    else
        round (toFloat window.width * 0.9) |> min 300


viewBoard : Window -> List Position -> Board -> Element Msg
viewBoard window selectedBlocks board =
    Element.el
        [ Element.height (Element.px (boardWidth window))
        , Element.width (Element.px (boardWidth window))
        , Element.centerX
        , Border.solid
        , Border.width 1
        , Element.padding 1
        ]
    <|
        Element.row
            [ Element.height Element.fill
            , Element.width Element.fill
            , Element.spacing 1
            ]
            (List.indexedMap (viewColumn selectedBlocks) board)


font : Element.Attribute msg
font =
    Font.family
        [ Font.typeface "Raleway"
        , Font.sansSerif
        ]


viewScore : Game -> Element Msg
viewScore game =
    case game of
        Finished gs ->
            Element.column [ Element.centerX, Element.width Element.fill ]
                [ Element.el [ Element.centerX ] (text "No more moves")
                , Element.el [ Element.centerX ] (text ("Your final score is " ++ String.fromInt gs.score ++ " point(s)"))
                ]

        InProgress gs ->
            text (String.fromInt gs.score ++ " point(s)")


viewHeader : Model -> Element Msg
viewHeader model =
    let
        ( el, spacing, fontSize ) =
            if model.window.width < 820 then
                ( Element.column, 2, 20 )

            else
                ( Element.row, 40, 30 )
    in
    el
        [ Element.centerX
        , Element.padding 10
        , Element.width Element.fill
        , Background.color (Element.rgb255 40 40 40)
        , Element.spacing spacing
        ]
        [ Element.el [ Font.size 40, Element.centerX ] (text "SameGame")
        , Input.button [ Font.size fontSize, Element.centerX ] { label = text "New Game", onPress = Just NewGame }
        , Input.button [ Font.size fontSize, Element.centerX ] { label = text "Highscores", onPress = Just (Dialog (Just HighScores)) }
        , Input.button [ Font.size fontSize, Element.centerX ] { label = text "Rules", onPress = Just (Dialog (Just Rules)) }
        ]


viewModalDialog : String -> Element Msg -> Element Msg
viewModalDialog heading content =
    Element.el [ Background.color (Element.rgb255 102 102 102), Element.centerX, Element.padding 10 ] (Element.column [ Element.spacing 20, Element.width Element.fill ] [ Element.el [ Element.centerX, Font.size 30 ] (text heading), content ])
        |> Element.el
            [ Background.color (Element.rgba 0 0 0 0.5)
            , Element.htmlAttribute (Attributes.style "z-index" "20")
            , Element.width Element.fill
            , Element.height Element.fill
            , Events.onClick (Dialog Nothing)
            , Element.paddingEach { top = 80, left = 10, bottom = 10, right = 10 }
            , Element.scrollbars
            ]


viewRules : Element Msg
viewRules =
    Element.textColumn [ Font.size 25, Element.spacing 20, Element.width Element.fill ]
        [ Element.paragraph [] [ text "SameGame is a single player game. It is played on a two-dimensional 15 x 15 board filled with blocks of different colors." ]
        , Element.paragraph [] [ text "The player may remove groups of adjoining blocks of the same color from the board." ]
        , Element.paragraph [] [ text "When a group is removed, all the blocks above will fall down." ]
        , Element.paragraph [] [ text "If a column is cleared completely, the columns to the right will slide to the left to close the gap." ]
        , Element.paragraph [] [ text "The game ends when the board is either empty or the remaining blocks cannot be removed." ]
        , Element.paragraph [] [ text "Removing a group of n blocks will result in a score of (n-2)² points." ]
        , Element.paragraph [] [ text "If all blocks are removed from the board, the player will receive a bonus of 1000 points." ]
        , Element.paragraph [] [ text "If the game ends without clearing the board, the player will receive a penalty. The penalty is computed according to (n-2)² where n is the]number of stones left on the board." ]
        ]


viewSelectedDialog : Model -> Element Msg
viewSelectedDialog model =
    case model.modal of
        Just HighScores ->
            viewModalDialog "Top 10" (viewHighScores model)

        Just Rules ->
            viewModalDialog "SameGame Rules" viewRules

        Nothing ->
            Element.none


page : Model -> List (Element.Attribute Msg)
page model =
    [ Background.color (Element.rgb255 102 102 102)
    , Font.color (Element.rgb255 238 238 238)
    , Element.htmlAttribute <| Attributes.style "font-weight" "300"
    , font
    , Element.width Element.fill
    , Element.inFront (viewSelectedDialog model)
    , Element.height Element.fill
    ]


viewEnterName : Window -> String -> Element Msg
viewEnterName window name =
    let
        el =
            if window.width < 500 then
                Element.column

            else
                Element.row
    in
    el [ Element.spacing 20, Element.centerX, Element.width (Element.px (boardWidth window)) ]
        [ Input.username [ Font.color (Element.rgb255 102 102 102), Element.width Element.fill ] { onChange = NameInputChange, text = name, placeholder = Just (Input.placeholder [] (text "Enter your name")), label = Input.labelHidden "Name" }
        , Input.button [ Font.size 25, Element.padding 10, Element.width Element.fill, Background.color (Element.rgb255 80 80 80) ] { label = Element.el [ Element.centerX ] <| text "Submit Score", onPress = Just AddToLeaderBoard }
        ]


max : Int -> Int -> Int
max limit i =
    if i > limit then
        limit

    else
        i


min : Int -> Int -> Int
min limit i =
    if i < limit then
        limit

    else
        i


footer : Element Msg
footer =
    Element.column
        [ Element.spacing 5, Element.width Element.fill, Font.size 16, Element.padding 10 ]
        [ Element.link [ Element.centerX ] { url = "http://elm-lang.org/", label = text "Created with Elm" }
        , Element.el [ Element.centerX ] <| Element.link [] { url = "https://github.com/battermann/elm-samegame", label = text "GitHub Source Code" }
        ]


viewMain : Model -> List (Element Msg)
viewMain model =
    let
        fontSize =
            round (toFloat model.window.width / 18) |> max 40
    in
    case model.game of
        Finished _ ->
            [ Element.el [ Element.centerX, Font.size fontSize ] (viewScore model.game)
            , viewEnterName model.window (model.playerName |> Maybe.withDefault "")
            , viewBoard model.window model.selectedBlocks (SameGame.board model.game)
            , footer
            ]

        InProgress _ ->
            [ Element.el [ Element.centerX, Font.size 40 ] (viewScore model.game)
            , viewBoard model.window model.selectedBlocks (SameGame.board model.game)
            , footer
            ]


viewHighScore : Int -> HighScoreEntry -> Element Msg
viewHighScore position highScore =
    viewTableEntry (String.fromInt (position + 1) ++ ".")
        (String.slice 0 10 highScore.nickname)
        (String.fromInt highScore.score)


viewTableEntry : String -> String -> String -> Element Msg
viewTableEntry first second third =
    Element.row [ Element.spacing 10, Element.width Element.fill, Font.size 25 ]
        [ Element.el [ Element.width (Element.px 70) ] (text first)
        , Element.el [ Element.width Element.fill ] (text second)
        , Element.el [ Element.width (Element.px 70) ] (text third)
        ]


viewHighScores : Model -> Element Msg
viewHighScores model =
    case model.highScores of
        NotAsked ->
            Element.el [ Font.size 18, Element.centerX ] (text "No data requested")

        Loading ->
            Element.el [ Font.size 18, Element.centerX ] (text "Loading...")

        Failure _ ->
            Element.el [ Font.size 18, Element.centerX ] (text "Failed to load")

        Success highScores ->
            Element.column [ Element.width Element.fill, Element.spacing 12 ] (viewTableEntry "Place" "Name" "Score" :: (highScores |> List.sortBy .score |> List.reverse |> List.indexedMap viewHighScore))

        Reloading highScores ->
            Element.column [ Element.width Element.fill, Element.spacing 12 ] (viewTableEntry "Place" "Name" "Score" :: (highScores |> List.sortBy .score |> List.reverse |> List.indexedMap viewHighScore))


view : Model -> Html Msg
view model =
    Element.layout (page model) <|
        Element.column [ Element.centerX, Element.width Element.fill, Element.height Element.fill, Element.spacing 20 ]
            (viewHeader model :: viewMain model)



---- PROGRAM ----


main : Program () Model Msg
main =
    Browser.element
        { view = view
        , init = \_ -> init
        , update = update
        , subscriptions = always <| Browser.Events.onResize WindowResize
        }
