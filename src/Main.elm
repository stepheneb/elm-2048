module Main exposing (Model, Msg(..), init, main, update, view)

import Array
import Browser
import Browser.Events as Events
import Browser.Navigation as Nav
import Html exposing (Html, a, button, div, form, h1, hr, img, p, strong, text)
import Html.Attributes exposing (class, href, id, src, target)
import Html.Events exposing (custom, on, onClick)
import Html.Keyed as Keyed
import Html.Lazy exposing (lazy)
import Json.Decode as Json
import Process
import Random
import Set
import Task
import Url



---- MODEL ----


type alias Model =
    { tiles : List Tile
    , key : Nav.Key
    , url : Url.Url
    , score : Int
    , bestScore : Int
    , nextTileKey : Int
    }


maximumNumberOfTiles : Int
maximumNumberOfTiles =
    16


type alias Tile =
    { value : Int
    , row : Int
    , col : Int
    , index : Int
    , new : Bool
    , merged : Bool
    , key : Int
    }


init : () -> Url.Url -> Nav.Key -> ( Model, Cmd Msg )
init flags url key =
    ( initialModel url key, generateNewTile [] )


initialModel : Url.Url -> Nav.Key -> Model
initialModel url key =
    { tiles = []
    , url = url
    , key = key
    , score = 0
    , bestScore = 0
    , nextTileKey = 1
    }



--- Tile generation


generateNewTile : List Tile -> Cmd Msg
generateNewTile tiles =
    if List.length tiles < maximumNumberOfTiles then
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
    , index = indx
    , new = True
    , merged = False
    , key = 0
    }


valueFrom : Float -> Int
valueFrom num =
    if num > 0.9 then
        4

    else
        2



---- UPDATE ----


type Msg
    = NoOp
    | NewGame
    | NewTile
    | AddTile Tile
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
            ( { model | tiles = [] }
            , generateNewTile []
            )

        NewTile ->
            ( model
            , generateNewTile model.tiles
            )

        AddTile tile ->
            ( updateScores
                { model
                    | nextTileKey = model.nextTileKey + 1
                    , tiles = addTile tile model |> sortTilesByRowsCols
                }
            , Cmd.none
            )

        MoveUp ->
            ( { model | tiles = moveUp model.tiles }
            , newTileLater
            )

        MoveDown ->
            ( { model | tiles = moveDown model.tiles }
            , newTileLater
            )

        MoveLeft ->
            ( { model | tiles = moveLeft model.tiles }
            , newTileLater
            )

        MoveRight ->
            ( { model | tiles = moveRight model.tiles }
            , newTileLater
            )

        LinkClicked urlRequest ->
            case urlRequest of
                Browser.Internal url ->
                    ( model, Nav.pushUrl model.key (Url.toString url) )

                Browser.External href ->
                    ( model, Nav.load href )

        UrlChanged url ->
            ( { model | url = url }
            , Cmd.none
            )


newTileLater : Cmd Msg
newTileLater =
    Process.sleep 300 |> Task.perform (always NewTile)


addTile : Tile -> Model -> List Tile
addTile tile model =
    { tile | key = model.nextTileKey } :: List.map (\t -> { t | new = False }) model.tiles


updateScores : Model -> Model
updateScores model =
    let
        score =
            List.foldl (+) 0 <| List.map .value model.tiles
    in
    { model
        | score = score
        , bestScore = max score model.bestScore
    }



--- Tile manipulation


removeMergedStatus : List Tile -> List Tile
removeMergedStatus tiles =
    List.map (\t -> { t | merged = False }) tiles


moveUp : List Tile -> List Tile
moveUp tiles =
    removeMergedStatus tiles
        |> tilesInColumns Normal
        |> List.map squashUp
        |> List.map mergeTiles
        |> List.map squashUp
        |> List.concat
        |> sortTilesByRowsCols


squashUp : List Tile -> List Tile
squashUp tiles =
    List.map2
        (\t r -> { t | row = r })
        tiles
        (List.range 1 4)


moveDown : List Tile -> List Tile
moveDown tiles =
    removeMergedStatus tiles
        |> tilesInColumns Reversed
        |> List.map squashDown
        |> List.map mergeTiles
        |> List.map squashDown
        |> List.concat
        |> sortTilesByRowsCols


squashDown : List Tile -> List Tile
squashDown tiles =
    List.map2
        (\t r -> { t | row = r })
        tiles
        (List.reverse <| List.range 1 4)


moveLeft : List Tile -> List Tile
moveLeft tiles =
    removeMergedStatus tiles
        |> tilesInRows Normal
        |> List.map squashLeft
        |> List.map mergeTiles
        |> List.map squashLeft
        |> List.concat
        |> sortTilesByRowsCols


squashLeft : List Tile -> List Tile
squashLeft tiles =
    List.map2
        (\t c -> { t | col = c })
        tiles
        (List.range 1 4)


moveRight : List Tile -> List Tile
moveRight tiles =
    removeMergedStatus tiles
        |> tilesInRows Reversed
        |> List.map squashRight
        |> List.map mergeTiles
        |> List.map squashRight
        |> List.concat
        |> sortTilesByRowsCols


squashRight : List Tile -> List Tile
squashRight tiles =
    List.map2
        (\t c -> { t | col = c })
        tiles
        (List.reverse <| List.range 1 4)


mergeTiles : List Tile -> List Tile
mergeTiles tiles =
    case tiles of
        t1 :: t2 :: rest ->
            mergeTilesHelp [] t1 t2 rest

        _ ->
            tiles


mergeTilesHelp : List Tile -> Tile -> Tile -> List Tile -> List Tile
mergeTilesHelp checked t1 t2 rest =
    case rest of
        t3 :: t4 :: ts ->
            if t1.value == t2.value then
                mergeTilesHelp ({ t1 | value = t1.value * 2, merged = True } :: checked) t3 t4 ts

            else
                mergeTilesHelp (t1 :: checked) t2 t3 (t4 :: ts)

        [ t3 ] ->
            if t1.value == t2.value then
                List.reverse
                    (t3 :: { t1 | value = t1.value * 2, merged = True } :: checked)

            else
                mergeTilesHelp (t1 :: checked) t2 t3 []

        [] ->
            if t1.value == t2.value then
                List.reverse ({ t1 | value = t1.value * 2, merged = True } :: checked)

            else
                List.reverse (t2 :: t1 :: checked)



-- Generate Array of empty location indices


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
    List.map .index tiles
        |> Set.fromList


allIndicesSet : Set.Set Int
allIndicesSet =
    Set.fromList <| List.range 1 16



--- Sorting list of tiles


type SortDirection
    = Normal
    | Reversed


tilesInColumns : SortDirection -> List Tile -> List (List Tile)
tilesInColumns direction tiles =
    List.map
        (\list ->
            case direction of
                Normal ->
                    identity list

                Reversed ->
                    List.reverse list
        )
        [ List.filter (\t -> t.col == 1) tiles
            |> List.sortWith colOrder
        , List.filter (\t -> t.col == 2) tiles
            |> List.sortWith colOrder
        , List.filter (\t -> t.col == 3) tiles
            |> List.sortWith colOrder
        , List.filter (\t -> t.col == 4) tiles
            |> List.sortWith colOrder
        ]


colOrder : Tile -> Tile -> Order
colOrder t1 t2 =
    if t1.col > t2.col then
        GT

    else if t1.col < t2.col then
        LT

    else
        EQ


tilesInRows : SortDirection -> List Tile -> List (List Tile)
tilesInRows direction tiles =
    List.map
        (\list ->
            case direction of
                Normal ->
                    identity list

                Reversed ->
                    List.reverse list
        )
        [ List.filter (\t -> t.row == 1) tiles
            |> List.sortWith rowOrder
        , List.filter (\t -> t.row == 2) tiles
            |> List.sortWith rowOrder
        , List.filter (\t -> t.row == 3) tiles
            |> List.sortWith rowOrder
        , List.filter (\t -> t.row == 4) tiles
            |> List.sortWith rowOrder
        ]


rowOrder : Tile -> Tile -> Order
rowOrder t1 t2 =
    if t1.row > t2.row then
        GT

    else if t1.row < t2.row then
        LT

    else
        EQ


sortTilesByRowsCols : List Tile -> List Tile
sortTilesByRowsCols tiles =
    List.sortWith rowColOrder tiles
        |> List.map (\t -> { t | index = rowIndex t.row t.col })


rowColOrder : Tile -> Tile -> Order
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


rowIndex : Int -> Int -> Int
rowIndex row col =
    col + (row - 1) * 4


colIndex : Int -> Int -> Int
colIndex row col =
    row + (col - 1) * 4



--- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Events.onKeyDown keyDecoder


keyDecoder : Json.Decoder Msg
keyDecoder =
    Json.map toDirection (Json.field "key" Json.string)


toDirection : String -> Msg
toDirection str =
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



---- VIEW ----


view : Model -> Browser.Document Msg
view model =
    { title = "Elm 2048"
    , body =
        [ div [ class "container" ]
            [ gameHeader model
            , aboveGame
            , div [ class "game-container" ]
                [ gameMessage
                , gridContainer
                , tileContainer model.tiles
                ]
            , gameExplanation
            , divider
            , gameNotes
            , divider
            , gameFooter
            ]
        ]
    }



--- Tiles


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



--- Playing grid


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



--- Above and below the playing area


gameHeader : Model -> Html Msg
gameHeader model =
    div [ class "heading" ]
        [ h1 [ class "title" ]
            [ text "Elm 2048" ]
        , div [ class "scores-container" ]
            [ div [ class "score-container" ]
                [ text <| String.fromInt <| model.score ]
            , div [ class "best-container" ]
                [ text <| String.fromInt <| model.bestScore ]
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


gameMessage : Html Msg
gameMessage =
    div [ class "game-message" ]
        [ p []
            []
        , div [ class "lower" ]
            [ a [ class "keep-playing-button" ]
                [ text "Keep going" ]
            , a [ class "retry-button" ]
                [ text "Try again" ]
            ]
        ]


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



---- PROGRAM ----


main : Program () Model Msg
main =
    Browser.application
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        , onUrlChange = UrlChanged
        , onUrlRequest = LinkClicked
        }
