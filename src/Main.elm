port module Main exposing (main)

import Array
import Browser
import Browser.Events as Events
import Browser.Navigation as Nav
import Html exposing (Html, a, button, div, form, h1, hr, img, p, strong, text)
import Html.Attributes exposing (class, href, id, src, target)
import Html.Events exposing (custom, on, onClick)
import Html.Keyed as Keyed
import Html.Lazy exposing (lazy)
import Json.Decode as Decode
import Json.Encode as Encode
import Process
import Random
import Set
import Task
import Url



---- PROGRAM ----


main : Program Decode.Value Model Msg
main =
    Browser.application
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        , onUrlChange = UrlChanged
        , onUrlRequest = LinkClicked
        }


init : Decode.Value -> Url.Url -> Nav.Key -> ( Model, Cmd Msg )
init flags url navKey =
    let
        model =
            initialModel flags url navKey
    in
    ( model, startGame model )


startGame : Model -> Cmd Msg
startGame model =
    if List.length model.gs.tiles < 2 then
        generateNewTile model.gs.tiles

    else
        Cmd.none



---- MODEL ----


type alias Model =
    { navKey : Nav.Key
    , url : Url.Url
    , gs : GameState
    }


type alias GameState =
    { tiles : List Tile
    , score : Int
    , bestScore : Int
    , status : Status
    , nextTileKey : Int
    }


type Status
    = Playing
    | Over
    | Won
    | KeepPlaying


type alias Tile =
    { value : Int
    , row : Int
    , col : Int
    , locIndex : Int
    , new : Bool
    , merged : Bool
    , moved : Bool
    , key : Int
    }


maxTiles : Int
maxTiles =
    16


initialModel : Decode.Value -> Url.Url -> Nav.Key -> Model
initialModel flags url navKey =
    { url = url
    , navKey = navKey
    , gs = parseGameState flags
    }



--- Decoding saved GameState
--- Parsing JSON GameState loaded through flags


parseGameState : Decode.Value -> GameState
parseGameState flags =
    case decodeGameState flags of
        Ok gameState ->
            gameState

        Err e ->
            defaultGameState


defaultGameState : GameState
defaultGameState =
    { tiles = []
    , score = 0
    , bestScore = 0
    , status = Playing
    , nextTileKey = 1
    }



--- Decoding JSON GameState


decodeGameState : Decode.Value -> Result Decode.Error GameState
decodeGameState flags =
    Decode.decodeValue gameStateDecoder flags


gameStateDecoder : Decode.Decoder GameState
gameStateDecoder =
    Decode.map5 GameState
        (Decode.field "tiles" tileListDecoder)
        (Decode.field "score" Decode.int)
        (Decode.field "bestScore" Decode.int)
        (Decode.field "status" statusDecoder)
        (Decode.field "nextTileKey" Decode.int)


tileListDecoder : Decode.Decoder (List Tile)
tileListDecoder =
    Decode.list tileDecoder


tileDecoder : Decode.Decoder Tile
tileDecoder =
    Decode.map8 Tile
        (Decode.field "value" Decode.int)
        (Decode.field "row" Decode.int)
        (Decode.field "col" Decode.int)
        (Decode.field "locIndex" Decode.int)
        (Decode.field "new" Decode.bool)
        (Decode.field "merged" Decode.bool)
        (Decode.field "moved" Decode.bool)
        (Decode.field "key" Decode.int)


statusDecoder : Decode.Decoder Status
statusDecoder =
    Decode.string
        |> Decode.andThen (fromResult << parseStatus)


parseStatus : String -> Result String Status
parseStatus string =
    case string of
        "Playing" ->
            Ok Playing

        "Over" ->
            Ok Over

        "Won" ->
            Ok Won

        "KeepPlaying" ->
            Ok KeepPlaying

        _ ->
            Err ("Invalid status: " ++ string)


fromResult : Result String a -> Decode.Decoder a
fromResult result =
    case result of
        Ok a ->
            Decode.succeed a

        Err errorMessage ->
            Decode.fail errorMessage



--- PORTS
--- incoming: touch swipe events turned into <arrow-key> Strings


port swipeDirectionArrow : (String -> msg) -> Sub msg



--- outgoing: GameState


port cacheGameState : Encode.Value -> Cmd msg


saveGameState : GameState -> Cmd Msg
saveGameState gs =
    cacheGameState (gameStateEncoder gs)


gameStateEncoder : GameState -> Encode.Value
gameStateEncoder gs =
    Encode.object
        [ ( "tiles", tilesEncoder gs.tiles )
        , ( "score", Encode.int gs.score )
        , ( "bestScore", Encode.int gs.bestScore )
        , ( "status", gameStatusEncoder gs.status )
        , ( "nextTileKey", Encode.int gs.nextTileKey )
        ]


gameStatusEncoder : Status -> Encode.Value
gameStatusEncoder status =
    let
        statusToStr =
            case status of
                Playing ->
                    "Playing"

                Over ->
                    "Over"

                Won ->
                    "Won"

                KeepPlaying ->
                    "KeepPlaying"
    in
    Encode.string statusToStr


tilesEncoder : List Tile -> Encode.Value
tilesEncoder tiles =
    Encode.list tileEncoder tiles


tileEncoder : Tile -> Encode.Value
tileEncoder tile =
    Encode.object
        [ ( "value", Encode.int tile.value )
        , ( "row", Encode.int tile.row )
        , ( "col", Encode.int tile.col )
        , ( "locIndex", Encode.int tile.locIndex )
        , ( "new", Encode.bool tile.merged )
        , ( "merged", Encode.bool tile.merged )
        , ( "moved", Encode.bool tile.moved )
        , ( "key", Encode.int tile.key )
        ]



--- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Events.onKeyDown keyDecoder
        , swipeDirectionArrow toDirectionMsg
        ]


keyDecoder : Decode.Decoder Msg
keyDecoder =
    Decode.map toDirectionMsg (Decode.field "key" Decode.string)


toDirectionMsg : String -> Msg
toDirectionMsg str =
    case str of
        "ArrowUp" ->
            MoveUp

        "ArrowDown" ->
            MoveDown

        "ArrowRight" ->
            MoveRight

        "ArrowLeft" ->
            MoveLeft

        _ ->
            NoOp



---- UPDATE ----


type Msg
    = NoOp
    | NewGame
    | NewTile
    | AddTile Tile
    | KeepGoing
    | MoveUp
    | MoveDown
    | MoveRight
    | MoveLeft
    | LinkClicked Browser.UrlRequest
    | UrlChanged Url.Url


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        NewGame ->
            let
                gs =
                    model.gs

                newGs =
                    { gs
                        | tiles = []
                        , score = 0
                        , nextTileKey = 1
                        , status = Playing
                    }
            in
            ( { model | gs = newGs }
            , generateNewTile newGs.tiles
            )

        NewTile ->
            ( model
            , generateNewTile model.gs.tiles
            )

        AddTile tile ->
            let
                gs =
                    model.gs

                newGs =
                    updateScoresAndGameStatus
                        { gs
                            | nextTileKey = gs.nextTileKey + 1
                            , tiles = addTile tile gs |> sortTilesByRowsCols
                        }
            in
            ( { model | gs = newGs }
            , if List.length newGs.tiles < 2 then
                generateNewTile newGs.tiles

              else
                saveGameState newGs
            )

        KeepGoing ->
            let
                gs =
                    model.gs

                newGs =
                    { gs | status = KeepPlaying }
            in
            ( { model | gs = newGs }
            , saveGameState newGs
            )

        MoveUp ->
            let
                newGs =
                    updateGameState model.gs moveUp
            in
            ( { model | gs = newGs }
            , newTileLaterIfTilesChanged newGs.tiles
            )

        MoveDown ->
            let
                newGs =
                    updateGameState model.gs moveDown
            in
            ( { model | gs = newGs }
            , newTileLaterIfTilesChanged newGs.tiles
            )

        MoveLeft ->
            let
                newGs =
                    updateGameState model.gs moveLeft
            in
            ( { model | gs = newGs }
            , newTileLaterIfTilesChanged newGs.tiles
            )

        MoveRight ->
            let
                newGs =
                    updateGameState model.gs moveRight
            in
            ( { model | gs = newGs }
            , newTileLaterIfTilesChanged newGs.tiles
            )

        LinkClicked urlRequest ->
            case urlRequest of
                Browser.Internal url ->
                    ( model, Nav.pushUrl model.navKey (Url.toString url) )

                Browser.External href ->
                    ( model, Nav.load href )

        UrlChanged url ->
            ( { model | url = url }
            , Cmd.none
            )



--- update: move tiles helper


updateGameState : GameState -> (List Tile -> List Tile) -> GameState
updateGameState gs func =
    { gs | tiles = func gs.tiles }



--- update: new tile helpers


newTileLaterIfTilesChanged : List Tile -> Cmd Msg
newTileLaterIfTilesChanged tiles =
    let
        changed =
            List.any (\t -> t.moved || t.merged) tiles
    in
    if changed then
        Process.sleep 200 |> Task.perform (always NewTile)

    else
        Cmd.none


addTile : Tile -> GameState -> List Tile
addTile tile gs =
    { tile | key = gs.nextTileKey }
        :: changeTiles (notNew >> notMoved) gs.tiles



--- update: score and status helpers


updateScoresAndGameStatus : GameState -> GameState
updateScoresAndGameStatus gs =
    let
        lastMoveScore =
            List.filter (\t -> t.merged) gs.tiles
                |> List.map .value
                |> List.foldl (+) 0

        score =
            gs.score + lastMoveScore
    in
    { gs
        | score = score
        , bestScore = max score gs.bestScore
        , status = gameStatus gs
    }


gameStatus : GameState -> Status
gameStatus gs =
    let
        gridFull =
            List.length gs.tiles == maxTiles

        any2048Tile =
            List.any (.value >> (==) 2048) gs.tiles
    in
    case gs.status of
        Playing ->
            if any2048Tile then
                Won

            else if gridFull then
                if movePossible gs.tiles then
                    Playing

                else
                    Over

            else
                Playing

        KeepPlaying ->
            if gridFull then
                if movePossible gs.tiles then
                    KeepPlaying

                else
                    Over

            else
                KeepPlaying

        _ ->
            gs.status


movePossible : List Tile -> Bool
movePossible tiles =
    let
        can move =
            lessThenMax (move tiles)
    in
    can moveUp || can moveLeft


lessThenMax : List Tile -> Bool
lessThenMax tiles =
    List.length tiles < maxTiles



--- update: tile generation with random placement and value


generateNewTile : List Tile -> Cmd Msg
generateNewTile tiles =
    if lessThenMax tiles then
        sortTilesByRowsCols tiles
            |> emptyLocationIndices
            |> newTileInEmptyLocation

    else
        Cmd.none


newTileInEmptyLocation : Array.Array Int -> Cmd Msg
newTileInEmptyLocation locationIndices =
    Random.generate AddTile (tileGenerator locationIndices)


tileGenerator : Array.Array Int -> Random.Generator Tile
tileGenerator locationIndices =
    Random.map2
        (\indx num ->
            Array.get (indx - 1) locationIndices
                |> Maybe.withDefault 1
                |> tileFromLocationIndex (valueFrom num)
        )
        (Random.int 1 (Array.length locationIndices))
        (Random.float 0 1)


tileFromLocationIndex : Int -> Int -> Tile
tileFromLocationIndex value indx =
    { value = value
    , row = (indx - 1) // 4 + 1
    , col = remainderBy 4 (indx - 1) + 1
    , locIndex = indx
    , new = True
    , merged = False
    , moved = False
    , key = 0
    }


valueFrom : Float -> Int
valueFrom num =
    if num > 0.9 then
        4

    else
        2



--- update: helpers for finding empty locations for new tiles


emptyLocationIndices : List Tile -> Array.Array Int
emptyLocationIndices tiles =
    if List.isEmpty tiles then
        allIndicesSet
            |> Set.toList
            |> Array.fromList

    else
        Set.diff allIndicesSet (placedIndicesSet tiles)
            |> Set.toList
            |> Array.fromList


placedIndicesSet : List Tile -> Set.Set Int
placedIndicesSet tiles =
    List.map .locIndex tiles
        |> Set.fromList


allIndicesSet : Set.Set Int
allIndicesSet =
    Set.fromList <| List.range 1 16



--- update: tile movement


changeTiles : (Tile -> Tile) -> List Tile -> List Tile
changeTiles func tiles =
    List.map func tiles


notNew : Tile -> Tile
notNew tile =
    { tile | new = False }


notMerged : Tile -> Tile
notMerged tile =
    { tile | merged = False }


notMoved : Tile -> Tile
notMoved tile =
    { tile | moved = False }



--- update: tile movement


moveUp : List Tile -> List Tile
moveUp tiles =
    let
        squashUp tilelist =
            List.map2
                (\t r -> maybeMoveTile t t.col r)
                tilelist
                (List.range 1 4)
    in
    changeTiles (notMerged >> notMoved) tiles
        |> tilesInColumns Normal
        |> List.map squashUp
        |> List.map mergeTiles
        |> List.map squashUp
        |> List.concat
        |> sortTilesByRowsCols


moveDown : List Tile -> List Tile
moveDown tiles =
    let
        squashDown tilelist =
            List.map2
                (\t r -> maybeMoveTile t t.col r)
                tilelist
                (List.reverse <| List.range 1 4)
    in
    changeTiles (notMerged >> notMoved) tiles
        |> tilesInColumns Reversed
        |> List.map squashDown
        |> List.map mergeTiles
        |> List.map squashDown
        |> List.concat
        |> sortTilesByRowsCols


moveLeft : List Tile -> List Tile
moveLeft tiles =
    let
        squashLeft tilelist =
            List.map2
                (\t c -> maybeMoveTile t c t.row)
                tilelist
                (List.range 1 4)
    in
    changeTiles (notMerged >> notMoved) tiles
        |> tilesInRows Normal
        |> List.map squashLeft
        |> List.map mergeTiles
        |> List.map squashLeft
        |> List.concat
        |> sortTilesByRowsCols


moveRight : List Tile -> List Tile
moveRight tiles =
    let
        squashRight tilelist =
            List.map2
                (\t c -> maybeMoveTile t c t.row)
                tilelist
                (List.reverse <| List.range 1 4)
    in
    changeTiles (notMerged >> notMoved) tiles
        |> tilesInRows Reversed
        |> List.map squashRight
        |> List.map mergeTiles
        |> List.map squashRight
        |> List.concat
        |> sortTilesByRowsCols


maybeMoveTile : Tile -> Int -> Int -> Tile
maybeMoveTile t c r =
    let
        newpos =
            t.col /= c || t.row /= r
    in
    if newpos then
        { t | col = c, row = r, moved = True }

    else
        t


mergeTiles : List Tile -> List Tile
mergeTiles tiles =
    case tiles of
        t1 :: t2 :: rest ->
            mergeTilesHelp [] t1 t2 rest

        _ ->
            tiles


mergeTilesHelp : List Tile -> Tile -> Tile -> List Tile -> List Tile
mergeTilesHelp checked t1 t2 rest =
    let
        merge t =
            { t | value = t.value * 2, merged = True }
    in
    case rest of
        t3 :: t4 :: ts ->
            if t1.value == t2.value then
                mergeTilesHelp (merge t2 :: checked) t3 t4 ts

            else
                mergeTilesHelp (t1 :: checked) t2 t3 (t4 :: ts)

        [ t3 ] ->
            if t1.value == t2.value then
                List.reverse
                    (t3 :: merge t2 :: checked)

            else
                mergeTilesHelp (t1 :: checked) t2 t3 []

        [] ->
            if t1.value == t2.value then
                List.reverse (merge t2 :: checked)

            else
                List.reverse (t2 :: t1 :: checked)



--- update: sorting tiles into lists of columns or rows


type SortDirection
    = Normal
    | Reversed


tilesInColumns : SortDirection -> List Tile -> List (List Tile)
tilesInColumns direction tiles =
    let
        colOrder t1 t2 =
            if t1.col > t2.col then
                GT

            else if t1.col < t2.col then
                LT

            else
                EQ

        sortedcol col =
            List.filter (\t -> t.col == col) tiles
                |> List.sortWith colOrder
    in
    List.map
        (\list ->
            case direction of
                Normal ->
                    identity list

                Reversed ->
                    List.reverse list
        )
        (List.map
            (\column -> sortedcol column)
            (List.range 1 4)
        )


tilesInRows : SortDirection -> List Tile -> List (List Tile)
tilesInRows direction tiles =
    let
        rowOrder t1 t2 =
            if t1.row > t2.row then
                GT

            else if t1.row < t2.row then
                LT

            else
                EQ

        sortedrow row =
            List.filter (\t -> t.row == row) tiles
                |> List.sortWith rowOrder
    in
    List.map
        (\list ->
            case direction of
                Normal ->
                    identity list

                Reversed ->
                    List.reverse list
        )
        (List.map
            (\row -> sortedrow row)
            (List.range 1 4)
        )


sortTilesByRowsCols : List Tile -> List Tile
sortTilesByRowsCols tiles =
    let
        rowIndex row col =
            col + (row - 1) * 4

        rowColOrder t1 t2 =
            let
                index1 =
                    rowIndex t1.row t1.col

                index2 =
                    rowIndex t2.row t2.col
            in
            if index1 > index2 then
                GT

            else if index1 < index2 then
                LT

            else
                EQ
    in
    List.sortWith rowColOrder tiles
        |> changeTiles (\t -> { t | locIndex = rowIndex t.row t.col })



---- VIEW ----


view : Model -> Browser.Document Msg
view model =
    { title = "Elm 2048"
    , body =
        [ div [ class "container" ]
            [ gameHeader model.gs
            , aboveGame
            , div [ class "game-container" ]
                [ gameMessage model.gs
                , gridContainer
                , tileContainer model.gs.tiles
                ]
            , gameExplanation
            , divider
            , gameNotes
            , divider
            , gameFooter
            ]
        ]
    }



--- view: playing grid


gridContainer : Html none
gridContainer =
    div [ class "grid-container" ]
        [ gridRow
        , gridRow
        , gridRow
        , gridRow
        ]


gridRow : Html none
gridRow =
    div [ class "grid-row" ]
        [ div [ class "grid-cell" ]
            []
        , div [ class "grid-cell" ]
            []
        , div [ class "grid-cell" ]
            []
        , div [ class "grid-cell" ]
            []
        ]



--- view: tile container and tiles


tileContainer : List Tile -> Html msg
tileContainer tiles =
    Keyed.node "div"
        [ class "tile-container" ]
        (listOfTiles tiles)


listOfTiles : List Tile -> List ( String, Html msg )
listOfTiles tiles =
    List.map singleKeyedTile tiles


singleKeyedTile : Tile -> ( String, Html msg )
singleKeyedTile t =
    ( String.fromInt t.key, lazy singleTile t )


singleTile : Tile -> Html msg
singleTile t =
    div
        [ class <| tileClassStr t ]
        [ div [ class "tile-inner" ]
            [ text <| String.fromInt t.value ]
        ]


tileClassStr : Tile -> String
tileClassStr t =
    let
        classStr =
            String.join " "
                [ "tile"
                , "tile-" ++ String.fromInt t.value
                , "tile-position-"
                    ++ String.fromInt t.col
                    ++ "-"
                    ++ String.fromInt t.row
                ]
    in
    classStr ++ newTileClassStr t ++ mergedTileClassStr t


newTileClassStr : Tile -> String
newTileClassStr t =
    case t.new of
        True ->
            " tile-new "

        False ->
            ""


mergedTileClassStr : Tile -> String
mergedTileClassStr t =
    case t.merged of
        True ->
            " tile-merged "

        False ->
            ""



--- view: above the game playing area


gameHeader : GameState -> Html Msg
gameHeader gs =
    div [ class "heading" ]
        [ h1 [ class "title" ]
            [ text "Elm 2048" ]
        , div [ class "scores-container" ]
            [ div [ class "score-container" ]
                [ text <| String.fromInt <| gs.score ]
            , div [ class "best-container" ]
                [ text <| String.fromInt <| gs.bestScore ]
            ]
        ]


aboveGame : Html Msg
aboveGame =
    div [ class "above-game" ]
        [ p [ class "game-intro" ]
            [ text "Join the numbers and get to the "
            , strong []
                [ text "2048 tile!" ]
            ]
        , button
            [ class "restart-button"
            , onClick NewGame
            ]
            [ text "New Game" ]
        ]



--- view: over the game area -- displayed when game over or won


gameMessage : GameState -> Html Msg
gameMessage gs =
    div [ class ("game-message" ++ gameStatusClassStr gs) ]
        [ p []
            [ text <| gameStatusMessage gs ]
        , div [ class "lower" ]
            [ button
                [ class "keep-playing-button"
                , onClick KeepGoing
                ]
                [ text "Keep going" ]
            , button
                [ class "retry-button"
                , onClick NewGame
                ]
                [ text "Try again" ]
            ]
        ]


gameStatusClassStr : GameState -> String
gameStatusClassStr gs =
    case gs.status of
        Playing ->
            ""

        KeepPlaying ->
            ""

        Over ->
            " game-over "

        Won ->
            " game-won"


gameStatusMessage : GameState -> String
gameStatusMessage gs =
    case gs.status of
        Playing ->
            ""

        KeepPlaying ->
            ""

        Over ->
            "Game Over"

        Won ->
            "You Won"



--- view: below the game playing area


gameExplanation : Html none
gameExplanation =
    p [ class "game-explanation" ]
        [ strong [ class "important" ]
            [ text "How to play: " ]
        , text "Use your "
        , strong []
            [ text "arrow keys" ]
        , text " to move the tiles. When two tiles with the same number touch, they "
        , strong []
            [ text "merge into one!" ]
        ]


gameNotes : Html none
gameNotes =
    p []
        [ strong [ class "important" ]
            [ text "Note: " ]
        , text "This is not the official version of 2048! It is an "
        , text "implementation of Gabriele Cirulli's "
        , a [ href "https://github.com/gabrielecirulli/2048" ]
            [ text "2048 game " ]
        , text "written in "
        , a [ href "https://elm-lang.org/" ]
            [ text "Elm" ]
        , text ". You can find the code for this Elm implementation here: "
        , a [ href "https://github.com/stepheneb/elm-2048" ]
            [ text "github.com/stepheneb/elm-2048" ]
        , text "."
        ]


gameFooter : Html none
gameFooter =
    p []
        [ text "Original 2048 created by "
        , a [ href "http://gabrielecirulli.com", target "_blank" ]
            [ text "Gabriele Cirulli. " ]
        , text "Based on "
        , a [ href "https://itunes.apple.com/us/app/1024!/id823499224", target "_blank" ]
            [ text "1024 by Veewo Studio " ]
        , text "and conceptually similar to "
        , a [ href "http://asherv.com/threes/", target "_blank" ]
            [ text "Threes by Asher Vollmer." ]
        ]


divider : Html none
divider =
    hr []
        []
