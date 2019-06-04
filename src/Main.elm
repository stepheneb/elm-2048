module Main exposing (Model, Msg(..), init, main, update, view)

import Array
import Browser
import Browser.Events as Events
import Browser.Navigation as Nav
import Html exposing (Html, a, button, div, form, h1, hr, img, p, strong, text)
import Html.Attributes exposing (class, href, id, src, target)
import Html.Events exposing (custom, on, onClick)
import Json.Decode as Json
import Random
import Set
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
    let
        _ =
            Debug.log ("tileGenerator: " ++ (String.fromInt <| Array.length locationIndices)) 1
    in
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
    }



---- UPDATE ----


type Msg
    = NoOp
    | NewGame
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

        AddTile tile ->
            ( { model | tiles = tile :: model.tiles }
            , Cmd.none
            )

        MoveUp ->
            ( { model
                | tiles =
                    sortTilesByRowsCols model.tiles
                        |> moveUp
              }
            , generateNewTile model.tiles
            )

        MoveDown ->
            ( { model
                | tiles =
                    sortTilesByRowsCols model.tiles
                        |> moveDown
              }
            , generateNewTile model.tiles
            )

        MoveLeft ->
            ( { model
                | tiles =
                    sortTilesByColsRows model.tiles
                        |> moveLeft
              }
            , generateNewTile model.tiles
            )

        MoveRight ->
            ( { model
                | tiles =
                    sortTilesByColsRows model.tiles
                        |> moveRight
              }
            , generateNewTile model.tiles
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



--- Tile manipulation


moveUp : List Tile -> List Tile
moveUp tiles =
    sortTilesByRowsCols <|
        List.concat <|
            [ List.map2
                (\t r -> { t | row = r })
                (List.filter (\t -> t.col == 1) tiles)
                (List.range 1 4)
            , List.map2
                (\t r -> { t | row = r })
                (List.filter (\t -> t.col == 2) tiles)
                (List.range 1 4)
            , List.map2
                (\t r -> { t | row = r })
                (List.filter (\t -> t.col == 3) tiles)
                (List.range 1 4)
            , List.map2
                (\t r -> { t | row = r })
                (List.filter (\t -> t.col == 4) tiles)
                (List.range 1 4)
            ]


moveDown : List Tile -> List Tile
moveDown tiles =
    sortTilesByRowsCols <|
        List.concat <|
            [ List.map2
                (\t r -> { t | row = r })
                (List.filter (\t -> t.col == 4) tiles)
                (List.reverse <| List.range 1 4)
            , List.map2
                (\t r -> { t | row = r })
                (List.filter (\t -> t.col == 3) tiles)
                (List.reverse <| List.range 1 4)
            , List.map2
                (\t r -> { t | row = r })
                (List.filter (\t -> t.col == 2) tiles)
                (List.reverse <| List.range 1 4)
            , List.map2
                (\t r -> { t | row = r })
                (List.filter (\t -> t.col == 1) tiles)
                (List.reverse <| List.range 1 4)
            ]


moveLeft : List Tile -> List Tile
moveLeft tiles =
    sortTilesByRowsCols <|
        List.concat <|
            [ List.map2
                (\t c -> { t | col = c })
                (List.filter (\t -> t.row == 1) tiles)
                (List.range 1 4)
            , List.map2
                (\t c -> { t | col = c })
                (List.filter (\t -> t.row == 2) tiles)
                (List.range 1 4)
            , List.map2
                (\t c -> { t | col = c })
                (List.filter (\t -> t.row == 3) tiles)
                (List.range 1 4)
            , List.map2
                (\t c -> { t | col = c })
                (List.filter (\t -> t.row == 4) tiles)
                (List.range 1 4)
            ]


moveRight : List Tile -> List Tile
moveRight tiles =
    sortTilesByRowsCols <|
        List.concat <|
            [ List.map2
                (\t c -> { t | col = c })
                (List.filter (\t -> t.row == 4) tiles)
                (List.reverse <| List.range 1 4)
            , List.map2
                (\t c -> { t | col = c })
                (List.filter (\t -> t.row == 3) tiles)
                (List.reverse <| List.range 1 4)
            , List.map2
                (\t c -> { t | col = c })
                (List.filter (\t -> t.row == 2) tiles)
                (List.reverse <| List.range 1 4)
            , List.map2
                (\t c -> { t | col = c })
                (List.filter (\t -> t.row == 1) tiles)
                (List.reverse <| List.range 1 4)
            ]



-- generate Array of empty location indices


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
    List.map (\t -> t.index) tiles
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
    List.map (\t -> singleTile t) tiles


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
    if t.new then
        "tile-new " ++ classStr

    else
        classStr



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
        , text "This is not the official version of 2048! It is an Elm implementation of Gabriele Cirulli's "
        , a [ href "https://github.com/gabrielecirulli/2048" ]
            [ text "2048 game" ]
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
