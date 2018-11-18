module SameGame exposing
    ( Block
    , BlockState(..)
    , Board
    , Color(..)
    , Column
    , Game(..)
    , GameState
    , Group
    , Position
    , board
    , groupPositions
    , newGame
    , play
    , score
    )

import List.Extra
import Random


type alias Position =
    { col : Int
    , row : Int
    }


type Color
    = Color Int


type BlockState
    = Empty
    | Filled Color


type alias Column =
    List BlockState


type alias Board =
    List Column


type alias Block =
    { position : Position
    , state : BlockState
    }


type alias Group =
    { color : Color
    , positions : List Position
    }


type alias GameState =
    { board : Board
    , score : Int
    }


type Game
    = InProgress GameState
    | Finished GameState



-- FUNCTIONS


board : Game -> Board
board game =
    case game of
        InProgress gs ->
            gs.board

        Finished gs ->
            gs.board


score : Game -> Int
score game =
    case game of
        InProgress gs ->
            gs.score

        Finished gs ->
            gs.score


bonus : Int
bonus =
    1000


calcScore : Group -> Int
calcScore group =
    (List.length group.positions - 2) ^ 2


penalty : Int -> Int
penalty numberOfStonesLeft =
    -((numberOfStonesLeft - 2) ^ 2)


getBlockState : Board -> Position -> BlockState
getBlockState gameBoard pos =
    gameBoard
        |> List.Extra.getAt pos.col
        |> Maybe.andThen (List.Extra.getAt pos.row)
        |> Maybe.withDefault Empty


left : Position -> Position
left position =
    { position | col = position.col - 1 }


right : Position -> Position
right position =
    { position | col = position.col + 1 }


up : Position -> Position
up position =
    { position | row = position.row + 1 }


down : Position -> Position
down position =
    { position | row = position.row - 1 }


findAdjacentWithSameColor : Board -> Color -> Position -> List Position
findAdjacentWithSameColor gameBoard color position =
    [ up position, right position, down position, left position ]
        |> List.filter (getBlockState gameBoard >> (==) (Filled color))


getBlocks : Board -> List Block
getBlocks =
    List.indexedMap (\i col -> col |> List.indexedMap (\j block -> { position = { col = i, row = j }, state = block })) >> List.concat


hasAdjacentWithSameColor : Board -> Block -> Bool
hasAdjacentWithSameColor gameBoard block =
    case block.state of
        Filled c ->
            block.position |> findAdjacentWithSameColor gameBoard c |> (not << List.isEmpty)

        _ ->
            False


hasValidMoves : Board -> Bool
hasValidMoves gameBoard =
    getBlocks gameBoard |> List.any (hasAdjacentWithSameColor gameBoard)


totalNumberOfBlocks : Board -> Int
totalNumberOfBlocks =
    let
        countOne state =
            case state of
                Filled _ ->
                    1

                Empty ->
                    0
    in
    List.map (List.map countOne >> List.sum) >> List.sum


columnIsEmpty : Column -> Bool
columnIsEmpty =
    List.head >> Maybe.withDefault Empty >> (==) Empty


isEmpty : Board -> Bool
isEmpty =
    List.all columnIsEmpty


evaluateGameState : GameState -> Game
evaluateGameState gameState =
    if gameState.board |> hasValidMoves then
        InProgress gameState

    else if gameState.board |> isEmpty then
        Finished { gameState | score = gameState.score + bonus }

    else
        Finished { gameState | score = gameState.score + (gameState.board |> totalNumberOfBlocks |> penalty) }


find : Board -> List Position -> Color -> List Position -> List Position
find gameBoard positions color group =
    case positions of
        [] ->
            group

        p :: ps ->
            let
                blocksToAddToGroup =
                    p
                        |> findAdjacentWithSameColor gameBoard color
                        |> List.filter (\pos -> not (List.any ((==) pos) (ps ++ group)))
            in
            find gameBoard (blocksToAddToGroup ++ ps) color (p :: group)


getGroup : Board -> Position -> Maybe Group
getGroup gameBoard position =
    case getBlockState gameBoard position of
        Filled color ->
            let
                touchingPositionsOfSameColor =
                    find gameBoard [ position ] color []
            in
            if List.length touchingPositionsOfSameColor > 1 then
                Just { color = color, positions = touchingPositionsOfSameColor }

            else
                Nothing

        Empty ->
            Nothing


groupPositions : Game -> Position -> List Position
groupPositions game position =
    case game of
        Finished _ ->
            []

        InProgress gs ->
            getGroup gs.board position
                |> Maybe.map .positions
                |> Maybe.withDefault []


removeGroup : Group -> Board -> Board
removeGroup group gameBoard =
    let
        fillWithEmpty n col2 =
            col2 ++ List.repeat (n - List.length col2) Empty

        fillWithEmptyColumns n m cols =
            cols ++ List.repeat (n - List.length cols) (List.repeat m Empty)

        columnHeight =
            List.head >> Maybe.map List.length >> Maybe.withDefault 0
    in
    gameBoard
        |> List.indexedMap
            (\i col ->
                col
                    |> List.indexedMap (\j block -> { position = { col = i, row = j }, state = block })
                    |> List.filter (\block -> group.positions |> (not << List.any ((==) block.position)))
                    |> List.map .state
                    |> fillWithEmpty (List.length col)
            )
        |> List.filter (columnIsEmpty >> not)
        |> fillWithEmptyColumns (List.length gameBoard) (columnHeight gameBoard)


applyPlay : Position -> GameState -> GameState
applyPlay pos gameState =
    case getGroup gameState.board pos of
        Just g ->
            { board = gameState.board |> removeGroup g, score = gameState.score + calcScore g }

        _ ->
            gameState


play : Position -> Game -> Game
play pos game =
    case game of
        InProgress gameState ->
            gameState |> applyPlay pos |> evaluateGameState

        _ ->
            game


newGame : Int -> Game
newGame seed =
    let
        colorGenerator =
            Random.int 0 4 |> Random.map (Color >> Filled)

        columnGenerator =
            Random.list 15 colorGenerator

        boardGenerator =
            Random.list 15 columnGenerator

        ( b, _ ) =
            Random.step boardGenerator (Random.initialSeed seed)
    in
    evaluateGameState { board = b, score = 0 }
