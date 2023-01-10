port module Main exposing (main)

import Browser
import Browser.Events as Events
import Browser.Navigation as Nav
import Html exposing (Html, a, button, div, h1, hr, img, p, strong, text)
import Html.Attributes exposing (class, href, id, src, target)
import Html.Events exposing (onClick)
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


winningTileValue : Int
winningTileValue =
    2048


maxTiles : Int
maxTiles =
    16



---- APPLICATION STARTUP ----


init : Decode.Value -> Url.Url -> Nav.Key -> ( Model, Cmd Msg )
init flags url navKey =
    let
        model =
            initialModel flags url navKey
    in
    ( model, startGame model )


initialModel : Decode.Value -> Url.Url -> Nav.Key -> Model
initialModel flags url navKey =
    { url = url
    , navKey = navKey
    , gs = parseGameState flags
    }


startGame : Model -> Cmd Msg
startGame model =
    if lessThenTwo model.gs.tiles then
        generateNewTile model.gs.tiles

    else
        Cmd.none


lessThenTwo : List a -> Bool
lessThenTwo a =
    List.length a < 2



--- application startup: load saved GameState via flags or generate default GameState


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


newGameState : GameState -> GameState
newGameState gs =
    let
        newGs =
            defaultGameState
    in
    { newGs | bestScore = gs.bestScore }



--- application startup: decode JSON GameState


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



---- PORTS ----
--- incoming: touch swipe events as { key: <arrow-key-str> objects.


port swipeDirectionArrow : (Encode.Value -> msg) -> Sub msg



--- outgoing: cache GameState in localStorage


port cacheGameState : Encode.Value -> Cmd msg


saveGameState : GameState -> Cmd Msg
saveGameState gs =
    cacheGameState (gameStateEncoder gs)



--- outgoing: encode GameState into JSON


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



---- SUBSCRIPTIONS ----


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Events.onKeyDown keyDecoderToMoveMsg
        , swipeDirectionArrow handleSwipe
        ]


type Direction
    = Up
    | Down
    | Left
    | Right


handleSwipe : Encode.Value -> Msg
handleSwipe value =
    case Decode.decodeValue directionDecoder value of
        Ok maybeDirection ->
            Move maybeDirection

        Err _ ->
            Move Nothing


keyDecoderToMoveMsg : Decode.Decoder Msg
keyDecoderToMoveMsg =
    Decode.map Move directionDecoder


directionDecoder : Decode.Decoder (Maybe Direction)
directionDecoder =
    Decode.andThen
        (\str ->
            case str of
                "ArrowUp" ->
                    Decode.succeed (Just Up)

                "ArrowDown" ->
                    Decode.succeed (Just Down)

                "ArrowRight" ->
                    Decode.succeed (Just Right)

                "ArrowLeft" ->
                    Decode.succeed (Just Left)

                _ ->
                    Decode.succeed Nothing
        )
        (Decode.field "key" Decode.string)



---- UPDATE ----


type Msg
    = NewGame
    | GenerateNewTile
    | AddTile Tile
    | KeepGoing
    | Move (Maybe Direction)
    | LinkClicked Browser.UrlRequest
    | UrlChanged Url.Url


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        gs =
            model.gs
    in
    case msg of
        NewGame ->
            let
                newGs =
                    newGameState gs
            in
            ( { model | gs = newGs }
            , generateNewTile newGs.tiles
            )

        GenerateNewTile ->
            ( model
            , generateNewTile gs.tiles
            )

        AddTile tile ->
            let
                newGs =
                    addTile gs tile
                        |> updateScoresAndGameStatus
            in
            ( { model | gs = newGs }
            , if lessThenTwo newGs.tiles then
                generateNewTile newGs.tiles

              else
                saveGameState newGs
            )

        KeepGoing ->
            let
                newGs =
                    { gs | status = KeepPlaying }
            in
            ( { model | gs = newGs }
            , saveGameState newGs
            )

        Move maybeDirection ->
            case maybeDirection of
                Just direction ->
                    updateWithMove model direction

                Nothing ->
                    ( model, Cmd.none )

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



--- update: move tiles helpers


updateWithMove : Model -> Direction -> ( Model, Cmd Msg )
updateWithMove model direction =
    let
        playing =
            userIsPlaying model.gs.status

        move =
            case direction of
                Up ->
                    moveUp

                Down ->
                    moveDown

                Left ->
                    moveLeft

                Right ->
                    moveRight

        newGs =
            updateGameState model.gs move
    in
    if playing then
        ( { model | gs = newGs }
        , possibleNewTile newGs
        )

    else
        ( model, Cmd.none )


updateGameState : GameState -> (List Tile -> List Tile) -> GameState
updateGameState gs func =
    let
        newGs =
            { gs
                | tiles =
                    changeTiles (notMerged >> notMoved) gs.tiles
                        |> func
                        |> sortTilesByRowsCols
            }
    in
    { newGs | status = gameStatus newGs }



--- update: new tile helpers


possibleNewTile : GameState -> Cmd Msg
possibleNewTile gs =
    let
        changed =
            List.any (\t -> t.moved || t.merged) gs.tiles
    in
    if userIsPlaying gs.status then
        if changed then
            Process.sleep 100 |> Task.perform (always GenerateNewTile)

        else
            Cmd.none

    else
        saveGameState gs


addTile : GameState -> Tile -> GameState
addTile gs tile =
    let
        newGs =
            { gs | nextTileKey = gs.nextTileKey + 1 }

        newTiles =
            { tile | key = gs.nextTileKey }
                :: changeTiles (notNew >> notMoved) newGs.tiles
                |> sortTilesByRowsCols
    in
    { newGs | tiles = newTiles }



--- update: score and status helpers


updateScoresAndGameStatus : GameState -> GameState
updateScoresAndGameStatus gs =
    let
        lastMoveScore =
            List.filter (\t -> t.merged) gs.tiles
                |> List.map .value
                |> List.foldl (+) 0

        newScore =
            gs.score + lastMoveScore
    in
    { gs
        | score = newScore
        , bestScore = max newScore gs.bestScore
        , status = gameStatus gs
    }


userIsPlaying : Status -> Bool
userIsPlaying status =
    case status of
        Playing ->
            True

        KeepPlaying ->
            True

        _ ->
            False


gameStatus : GameState -> Status
gameStatus gs =
    let
        gridFull =
            List.length gs.tiles == maxTiles

        anyWinningTile =
            List.any (.value >> (==) winningTileValue) gs.tiles
    in
    case gs.status of
        Playing ->
            if anyWinningTile then
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
    let
        locations =
            sortTilesByRowsCols tiles
                |> emptyLocations
    in
    if lessThenMax tiles then
        case locations of
            [] ->
                Cmd.none

            l :: ls ->
                Random.generate AddTile (tileGenerator l ls)

    else
        Cmd.none


tileGenerator : Int -> List Int -> Random.Generator Tile
tileGenerator firstLocation restOfLocations =
    let
        valueFrom num =
            if num > 0.9 then
                4

            else
                2
    in
    Random.map2
        (\locIndex num ->
            tileAtLocationIndex (valueFrom num) locIndex
        )
        (Random.uniform firstLocation restOfLocations)
        (Random.float 0 1)


emptyLocations : List Tile -> List Int
emptyLocations tiles =
    let
        allIndicesSet =
            Set.fromList <| List.range 1 maxTiles

        placedIndicesSet =
            List.map .locIndex tiles
                |> Set.fromList
    in
    if List.isEmpty tiles then
        List.range 1 maxTiles

    else
        Set.diff allIndicesSet placedIndicesSet
            |> Set.toList


tileAtLocationIndex : Int -> Int -> Tile
tileAtLocationIndex value indx =
    { value = value
    , row = (indx - 1) // 4 + 1
    , col = remainderBy 4 (indx - 1) + 1
    , locIndex = indx
    , new = True
    , merged = False
    , moved = False
    , key = 0
    }



--- update: tile movement


moveUp : List Tile -> List Tile
moveUp tiles =
    let
        squashUp ts =
            List.map2
                (\t r -> maybeMoveTile t t.col r)
                ts
                (List.range 1 4)
    in
    tilesInColumns Normal tiles
        |> List.map squashUp
        |> List.map mergeTiles
        |> List.map squashUp
        |> List.concat


moveDown : List Tile -> List Tile
moveDown tiles =
    let
        squashDown ts =
            List.map2
                (\t r -> maybeMoveTile t t.col r)
                ts
                (List.reverse <| List.range 1 4)
    in
    tilesInColumns Reversed tiles
        |> List.map squashDown
        |> List.map mergeTiles
        |> List.map squashDown
        |> List.concat


moveLeft : List Tile -> List Tile
moveLeft tiles =
    let
        squashLeft ts =
            List.map2
                (\t c -> maybeMoveTile t c t.row)
                ts
                (List.range 1 4)
    in
    tilesInRows Normal tiles
        |> List.map squashLeft
        |> List.map mergeTiles
        |> List.map squashLeft
        |> List.concat


moveRight : List Tile -> List Tile
moveRight tiles =
    let
        squashRight ts =
            List.map2
                (\t c -> maybeMoveTile t c t.row)
                ts
                (List.reverse <| List.range 1 4)
    in
    tilesInRows Reversed tiles
        |> List.map squashRight
        |> List.map mergeTiles
        |> List.map squashRight
        |> List.concat


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
                List.reverse (t3 :: merge t2 :: checked)

            else
                mergeTilesHelp (t1 :: checked) t2 t3 []

        [] ->
            if t1.value == t2.value then
                List.reverse (merge t2 :: checked)

            else
                List.reverse (t2 :: t1 :: checked)



--- update: tile change helpers


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



--- update: sort tiles into lists of columns or rows


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
    classStr ++ newTileClassStr t ++ mergedTileClassStr t ++ superTileClassStr t


newTileClassStr : Tile -> String
newTileClassStr t =
    if t.new then
        " tile-new "

    else
        ""


mergedTileClassStr : Tile -> String
mergedTileClassStr t =
    if t.merged then
        " tile-merged "

    else
        ""


superTileClassStr : Tile -> String
superTileClassStr t =
    if t.value > winningTileValue then
        " tile-super "

    else
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



--- view: over the game area (only displayed when game over or won)


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
        , text " or swipe on a touch screen to move the tiles. "
        , text "When two tiles with the same number touch, they "
        , strong []
            [ text "merge into one!" ]
        ]


gameNotes : Html none
gameNotes =
    p [ class "game-explanation" ]
        [ strong [ class "important" ]
            [ text "Note: " ]
        , text "This is not the official version of 2048! It is an "
        , text "implementation of Gabriele Cirulli's "
        , a [ href "https://github.com/gabrielecirulli/2048", target "_blank" ]
            [ text "2048 game " ]
        , text "written in "
        , a [ href "https://elm-lang.org/" ]
            [ text "Elm" ]
        , text ". You can find the code for this Elm implementation here: "
        , a [ href "https://github.com/stepheneb/elm-2048", target "_blank" ]
            [ text "github.com/stepheneb/elm-2048" ]
        , text ". "
        ]


gameFooter : Html none
gameFooter =
    p [ class "game-explanation" ]
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
    hr [] []
