module Main exposing (Model, Msg(..), init, main, update, view)

import Browser
import Browser.Dom
import Browser.Events
import Browser.Navigation
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
import List.Extra
import MD5
import Random
import SameGame exposing (Block, BlockState(..), Board, Color(..), Column, Game(..), Position)
import Task
import Url exposing (Url)
import Url.Builder
import Url.Parser exposing ((</>), (<?>), Parser(..))
import Url.Parser.Query as Query



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


type GameType
    = HighScoreGame Game
    | RandomGame Game


game : GameType -> Game
game gameType =
    case gameType of
        HighScoreGame g ->
            g

        RandomGame g ->
            g


play : Position -> GameType -> GameType
play position gameType =
    case gameType of
        HighScoreGame g ->
            SameGame.play position g |> HighScoreGame

        RandomGame g ->
            SameGame.play position g |> RandomGame


type alias Model =
    { game : GameType
    , selectedBlocks : List Position
    , modal : Maybe Modal
    , playerName : Maybe String
    , highScores : RemoteData String (List HighScoreEntry)
    , window : Window
    , location : Url
    , key : Browser.Navigation.Key
    }


toMsg : Browser.Dom.Viewport -> Msg
toMsg { viewport } =
    WindowResize (viewport.width |> round) (viewport.height |> round)


init : Url -> Browser.Navigation.Key -> ( Model, Cmd Msg )
init url key =
    initPage (Window 0 0) NotAsked url key (Cmd.batch [ Task.perform toMsg Browser.Dom.getViewport, highScores ])


initPage : Window -> RemoteData String (List HighScoreEntry) -> Url -> Browser.Navigation.Key -> Cmd Msg -> ( Model, Cmd Msg )
initPage window hs url key cmd =
    let
        makeModel g =
            { game = g
            , selectedBlocks = []
            , modal = Nothing
            , playerName = Nothing
            , highScores = hs
            , window = window
            , location = url
            , key = key
            }
    in
    case extractRoute url of
        Game 1 moves ->
            ( makeModel (moves |> List.foldl play (HighScoreGame (SameGame.newGame 1))), cmd )

        Game num moves ->
            ( makeModel (moves |> List.foldl play (RandomGame (SameGame.newGame num))), cmd )


type alias HighScoreEntry =
    { nickname : String
    , score : Int
    }



---- HTTP ----


add : Model -> Cmd Msg
add model =
    Http.post "https://cors-anywhere.herokuapp.com/https://omgleaderboards.appspot.com/add" (Http.stringBody "application/x-www-form-urlencoded" (data (SameGame.score (game model.game)) (model.playerName |> Maybe.withDefault ""))) (Decode.succeed "")
        |> Http.send AddToLeaderBoardResult


decodeHighScoreEntries : Decode.Decoder (List HighScoreEntry)
decodeHighScoreEntries =
    let
        singleEntry =
            Decode.map2 HighScoreEntry (Decode.at [ "nickname" ] Decode.string) (Decode.at [ "score" ] Decode.int)
    in
    Decode.at [ "scores", "alltime" ] (Decode.list singleEntry)


highScores : Cmd Msg
highScores =
    Http.get
        "https://cors-anywhere.herokuapp.com/https://omgleaderboards.appspot.com/get/71924738-62fe-4543-9d37-2e666bb27df8?timeframes=alltime&limit=10"
        decodeHighScoreEntries
        |> Http.send HighScoresResult



---- UPDATE ----


type Msg
    = SelectGroup Position
    | Dialog (Maybe Modal)
    | HighScoresResult (Result Http.Error (List HighScoreEntry))
    | NameInputChange String
    | AddToLeaderBoard
    | AddToLeaderBoardResult (Result Http.Error String)
    | WindowResize Int Int
    | RandomGameResult Int
    | NewRandomGame
    | ChangedUrl Url
    | ClickedLink Browser.UrlRequest


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SelectGroup position ->
            ( { model | selectedBlocks = SameGame.groupPositions (game model.game) position }, Cmd.none )

        Dialog (Just HighScores) ->
            case model.highScores of
                Success scores ->
                    ( { model | modal = Just HighScores, highScores = Reloading scores }, highScores )

                _ ->
                    ( { model | modal = Just HighScores, highScores = Loading }, highScores )

        Dialog modal ->
            ( { model | modal = modal }, Cmd.none )

        HighScoresResult (Err _) ->
            ( { model | highScores = Failure "Http Error" }, Cmd.none )

        HighScoresResult (Ok scores) ->
            ( { model | highScores = Success scores }, Cmd.none )

        NameInputChange name ->
            ( { model | playerName = Just name }, Cmd.none )

        AddToLeaderBoard ->
            ( { model
                | game = HighScoreGame (SameGame.newGame 1)
                , selectedBlocks = []
                , modal = Nothing
                , playerName = Nothing
              }
            , add model
            )

        AddToLeaderBoardResult _ ->
            ( model, Cmd.none )

        WindowResize width height ->
            ( { model | window = Window width height }, Cmd.none )

        RandomGameResult gameNumber ->
            ( model, Browser.Navigation.pushUrl model.key (Url.Builder.absolute [ "game", String.fromInt gameNumber ] []) )

        NewRandomGame ->
            ( model, Random.generate RandomGameResult (Random.int -2147483648 2147483647) )

        ClickedLink request ->
            case request of
                Browser.Internal url ->
                    ( model, Browser.Navigation.pushUrl model.key (Url.toString url) )

                Browser.External href ->
                    ( model, Browser.Navigation.load href )

        ChangedUrl url ->
            initPage model.window model.highScores url model.key Cmd.none



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


viewCell : Url -> List Position -> Int -> Int -> BlockState -> Element Msg
viewCell url selectedBlocks columnIndex rowIndex cellState =
    let
        position =
            { col = columnIndex, row = 14 - rowIndex }
    in
    case cellState of
        Filled color ->
            Element.link
                ([ Element.height Element.fill
                 , Element.width Element.fill
                 , viewColor (alpha selectedBlocks position) color
                 , Events.onMouseEnter (SelectGroup position)
                 ]
                    ++ cursor selectedBlocks position
                )
                { url = url |> addMove position, label = Element.none }

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


viewColumn : Url -> List Position -> Int -> Column -> Element Msg
viewColumn url selectedBlocks index column =
    column
        |> List.reverse
        |> List.indexedMap (viewCell url selectedBlocks index)
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


viewBoard : Window -> Url -> List Position -> Board -> Element Msg
viewBoard window url selectedBlocks board =
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
            (List.indexedMap (viewColumn url selectedBlocks) board)


font : Element.Attribute msg
font =
    Font.family
        [ Font.typeface "Raleway"
        , Font.sansSerif
        ]


viewScore : Game -> Element Msg
viewScore g =
    case g of
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
        , Input.button [ Font.size fontSize, Element.centerX ] { label = text "New Game", onPress = Just NewRandomGame }
        , Element.link [ Font.size fontSize, Element.centerX ] { url = "/game/1", label = text "Game 1" }
        , Input.button [ Font.size fontSize, Element.centerX ] { label = text "Highscores", onPress = Just (Dialog (Just HighScores)) }
        , Input.button [ Font.size fontSize, Element.centerX ] { label = text "Rules", onPress = Just (Dialog (Just Rules)) }
        ]


viewModalDialog : String -> Element Msg -> Element Msg
viewModalDialog heading content =
    Element.el [ Background.color (Element.rgb255 102 102 102), Element.centerX, Element.padding 20, Element.width Element.fill ] (Element.column [ Element.spacing 20, Element.width Element.fill ] [ Element.el [ Element.centerX, Font.size 30 ] (text heading), content ])
        |> Element.el
            [ Background.color (Element.rgba 0 0 0 0.5)
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
        , Element.paragraph [] [ text "If the game ends without clearing the board, the player will receive a penalty. The penalty is computed according to (n-2)² where n is the number of stones left on the board." ]
        ]


viewSelectedDialog : Model -> Element Msg
viewSelectedDialog model =
    case model.modal of
        Just HighScores ->
            viewModalDialog "Game 1 Highscores" (viewHighScores model)

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

        attributes =
            [ Font.size 25, Element.padding 10, Element.width Element.fill, Element.centerX ]

        button =
            if name |> (not << String.isEmpty) then
                Input.button (Background.color (Element.rgb255 80 80 80) :: attributes) { label = Element.el [ Element.centerX ] <| text "Submit Score", onPress = Just AddToLeaderBoard }

            else
                Element.el (Background.color (Element.rgb255 90 90 90) :: Font.color (Element.rgb255 150 150 150) :: attributes) (Element.el [ Element.centerX ] <| text "Submit Score")
    in
    el [ Element.spacing 20, Element.centerX, Element.width (Element.px (boardWidth window)) ]
        [ Input.username [ Font.color (Element.rgb255 102 102 102), Element.width Element.fill ] { onChange = NameInputChange, text = name, placeholder = Just (Input.placeholder [] (text "Enter your name")), label = Input.labelHidden "Name" }
        , button
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
        [ Element.spacing 5, Element.width Element.fill, Font.size 16, Element.padding 10, Background.color (Element.rgb255 102 102 102) ]
        [ Element.link [ Element.centerX ] { url = "http://elm-lang.org/", label = text "Created with Elm" }
        , Element.el [ Element.centerX ] <| Element.link [] { url = "https://github.com/battermann/elm-samegame", label = Element.row [] [ Element.el [ Element.centerX, Element.centerY ] (Element.html (Html.div [] [ Html.i [ Attributes.class "fab fa-github" ] [] ])), text " Source Code" ] }
        ]


viewMain : Model -> List (Element Msg)
viewMain model =
    let
        fontSize =
            round (toFloat model.window.width / 18) |> max 40
    in
    case model.game of
        RandomGame (Finished _) ->
            [ Element.el [ Element.centerX, Font.size fontSize ] (viewScore (game model.game))
            , viewBoard model.window model.location model.selectedBlocks (SameGame.board (game model.game))
            , Element.el [ Element.centerX, Font.size 16 ] (text "Only Game 1 supports Highscores")
            , footer
            ]

        RandomGame (InProgress _) ->
            [ Element.el [ Element.centerX, Font.size 40 ] (viewScore (game model.game))
            , viewBoard model.window model.location model.selectedBlocks (SameGame.board (game model.game))
            , Element.el [ Element.centerX, Font.size 16 ] (text "Only Game 1 supports Highscores")
            , footer
            ]

        HighScoreGame (Finished _) ->
            [ Element.el [ Element.centerX, Font.size fontSize ] (viewScore (game model.game))
            , viewEnterName model.window (model.playerName |> Maybe.withDefault "")
            , viewBoard model.window model.location model.selectedBlocks (SameGame.board (game model.game))
            , footer
            ]

        HighScoreGame (InProgress _) ->
            [ Element.el [ Element.centerX, Font.size 40 ] (viewScore (game model.game))
            , viewBoard model.window model.location model.selectedBlocks (SameGame.board (game model.game))
            , footer
            ]


viewHighScore : Int -> HighScoreEntry -> Element Msg
viewHighScore position highScore =
    viewTableEntry (String.fromInt (position + 1) ++ ".")
        highScore.nickname
        (String.fromInt highScore.score)


viewTableEntry : String -> String -> String -> Element Msg
viewTableEntry first second third =
    Element.row [ Element.spacing 10, Element.width Element.fill, Font.size 25 ]
        [ Element.el [ Element.width (Element.px 70) ] (text first)
        , Element.el [ Element.width Element.fill, Element.htmlAttribute (Attributes.style "overflow" "hidden") ] (text second)
        , Element.el [ Element.width (Element.px 70) ] (text third)
        ]


viewHighScores : Model -> Element Msg
viewHighScores model =
    let
        spinner =
            Element.el [ Element.centerX, Element.centerY ] (Element.html (Html.div [] [ Html.i [ Attributes.class "fas fa-spinner fa-spin" ] [] ]))

        width =
            toFloat model.window.width * 0.6 |> round |> min 280
    in
    case model.highScores of
        NotAsked ->
            Element.el [ Font.size 18, Element.centerX ] (text "No data requested")

        Loading ->
            Element.row [ Font.size 18, Element.centerX ] [ spinner, text " Loading..." ]

        Failure _ ->
            Element.el [ Font.size 18, Element.centerX ] (text "Failed to load")

        Success hs ->
            Element.column [ Element.width (Element.px width), Element.centerX, Element.spacing 12 ] (viewTableEntry "Place" "Name" "Score" :: (hs |> List.sortBy .score |> List.reverse |> List.indexedMap viewHighScore))

        Reloading hs ->
            Element.column [ Element.width (Element.px width), Element.centerX, Element.spacing 12 ] (viewTableEntry "Place" "Name" "Score" :: (hs |> List.sortBy .score |> List.reverse |> List.indexedMap viewHighScore))


view : Model -> Browser.Document Msg
view model =
    { title = "SameGame"
    , body =
        List.singleton <|
            Element.layout (page model) <|
                Element.column [ Element.centerX, Element.width Element.fill, Element.height Element.fill, Element.spacing 20 ]
                    (viewHeader model :: viewMain model)
    }



---- ROUTING ----


type Route
    = Game Int (List Position)


extractRoute : Url -> Route
extractRoute location =
    case Url.Parser.parse matchRoute location of
        Just route ->
            route

        Nothing ->
            Game 1 []


addMove : Position -> Url -> String
addMove position url =
    case extractRoute url of
        Game num moves ->
            Url.Builder.absolute [ "game", String.fromInt num ] ([ position ] |> List.append moves |> List.map encodeMove)


encodeMove : Position -> Url.Builder.QueryParameter
encodeMove { col, row } =
    Url.Builder.string "move" (String.join "," [ String.fromInt col, String.fromInt row ])


parseMove : String -> Maybe Position
parseMove move =
    case String.split "," move of
        x :: y :: [] ->
            Maybe.map2 Position (String.toInt x) (String.toInt y)

        _ ->
            Nothing


parseMoves : List String -> List Position
parseMoves =
    List.filterMap parseMove


matchRoute : Parser (Route -> a) a
matchRoute =
    Url.Parser.map Game (Url.Parser.s "game" </> Url.Parser.int <?> Query.custom "move" parseMoves)



---- PROGRAM ----


main : Program () Model Msg
main =
    Browser.application
        { view = view
        , init = \_ -> init
        , update = update
        , subscriptions = always <| Browser.Events.onResize WindowResize
        , onUrlChange = ChangedUrl
        , onUrlRequest = ClickedLink
        }
