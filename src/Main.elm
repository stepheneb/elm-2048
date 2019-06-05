module Main exposing (Model, Msg(..), init, main, update, view)

import Array
import Browser
import Browser.Events as Events
import Browser.Navigation as Nav
import Html exposing (Html, a, button, div, form, h1, hr, img, p, strong, text)
import Html.Attributes exposing (class, href, id, src, target)
import Html.Events exposing (custom, on, onClick)
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
    }


init : () -> Url.Url -> Nav.Key -> ( Model, Cmd Msg )
init flags url key =
    ( initialModel url key, generateNewTile [] )


initialModel : Url.Url -> Nav.Key -> Model
initialModel url key =
    { tiles = []
    , url = url
    , key = key
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
    Random.map
        (\indx ->
            Array.get (indx - 1) locationIndices
                |> Maybe.withDefault 1
                |> tileFromLocationIndex
        )
        (Random.int 1 (Array.length locationIndices))


tileFromLocationIndex : Int -> Tile
tileFromLocationIndex indx =
    { value = 2
    , row = (indx - 1) // 4 + 1
    , col = remainderBy 4 (indx - 1) + 1
    , index = indx
    , new = True
    , merged = False
    }



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
            ( { model | tiles = addTile tile model.tiles }
            , Cmd.none
            )

        MoveUp ->
            ( { model
                | tiles =
                    sortTilesByRowsCols model.tiles
                        |> moveUp
              }
            , newTileLater
            )

        MoveDown ->
            ( { model
                | tiles =
                    sortTilesByRowsCols model.tiles
                        |> moveDown
              }
            , newTileLater
            )

        MoveLeft ->
            ( { model
                | tiles =
                    sortTilesByColsRows model.tiles
                        |> moveLeft
              }
            , newTileLater
            )

        MoveRight ->
            ( { model
                | tiles =
                    sortTilesByColsRows model.tiles
                        |> moveRight
              }
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


addTile : Tile -> List Tile -> List Tile
addTile tile tiles =
    tile :: List.map (\t -> { t | new = False }) tiles



--- Tile manipulation


removeMergedStatus : List Tile -> List Tile
removeMergedStatus tiles =
    List.map (\t -> { t | merged = False }) tiles


moveUp : List Tile -> List Tile
moveUp tiles =
    removeMergedStatus tiles
        |> moveUpDownHelp range1to4
        |> sortTilesByRowsCols


moveDown : List Tile -> List Tile
moveDown tiles =
    removeMergedStatus tiles
        |> moveUpDownHelp range4to1
        |> sortTilesByRowsCols


moveUpDownHelp : List Int -> List Tile -> List Tile
moveUpDownHelp range tiles =
    List.concat <|
        [ contractColumn range 1 tiles
            |> mergeTiles
            |> contractColumn range 1
        , contractColumn range4to1 2 tiles
            |> mergeTiles
            |> contractColumn range 2
        , contractColumn range 3 tiles
            |> mergeTiles
            |> contractColumn range 3
        , contractColumn range 4 tiles
            |> mergeTiles
            |> contractColumn range 4
        ]


moveLeft : List Tile -> List Tile
moveLeft tiles =
    removeMergedStatus tiles
        |> moveLeftRightHelp range1to4
        |> sortTilesByRowsCols


moveRight : List Tile -> List Tile
moveRight tiles =
    removeMergedStatus tiles
        |> moveLeftRightHelp range4to1
        |> sortTilesByRowsCols


moveLeftRightHelp : List Int -> List Tile -> List Tile
moveLeftRightHelp range tiles =
    List.concat <|
        [ contractRow range 1 tiles
            |> mergeTiles
            |> contractRow range 1
        , contractRow range4to1 2 tiles
            |> mergeTiles
            |> contractRow range 2
        , contractRow range 3 tiles
            |> mergeTiles
            |> contractRow range 3
        , contractRow range 4 tiles
            |> mergeTiles
            |> contractRow range 4
        ]


range1to4 : List Int
range1to4 =
    List.range 1 4


range4to1 : List Int
range4to1 =
    List.reverse <| List.range 1 4


contractColumn : List Int -> Int -> List Tile -> List Tile
contractColumn range colnum tiles =
    List.map2
        (\t r -> { t | row = r })
        (List.filter (\t -> t.col == colnum) tiles)
        range


contractRow : List Int -> Int -> List Tile -> List Tile
contractRow range rownum tiles =
    List.map2
        (\t c -> { t | col = c })
        (List.filter (\t -> t.row == rownum) tiles)
        range


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
                [ { t1 | value = t1.value * 2, merged = True }, t3 ]

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


sortTilesByRowsCols : List Tile -> List Tile
sortTilesByRowsCols tiles =
    List.sortWith rowColOrder tiles
        |> List.map (\t -> { t | index = rowIndex t.row t.col })


sortTilesByColsRows : List Tile -> List Tile
sortTilesByColsRows tiles =
    List.sortWith colRowOrder tiles
        |> List.map (\t -> { t | index = colIndex t.row t.col })


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


colRowOrder t1 t2 =
    let
        index1 =
            colIndex t1.row t1.col

        index2 =
            colIndex t2.row t2.col
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
            [ gameHeader
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


tileContainer : List Tile -> Html Msg
tileContainer tiles =
    div [ class "tile-container" ]
        (listOfTiles tiles)


listOfTiles : List Tile -> List (Html Msg)
listOfTiles tiles =
    List.map singleTile tiles


singleTile : Tile -> Html Msg
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


gameHeader : Html Msg
gameHeader =
    div [ class "heading" ]
        [ h1 [ class "title" ]
            [ text "Elm 2048" ]
        , div [ class "scores-container" ]
            [ div [ class "score-container" ]
                [ text "0" ]
            , div [ class "best-container" ]
                [ text "0" ]
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
