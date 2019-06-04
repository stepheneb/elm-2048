module Main exposing (Model, Msg(..), init, main, update, view)

import Browser
import Browser.Events as Events
import Browser.Navigation as Nav
import Html exposing (Html, a, button, div, form, h1, hr, img, p, strong, text)
import Html.Attributes exposing (class, href, id, src, target)
import Html.Events exposing (custom, on, onClick)
import Json.Decode as Json
import Random
import Url



---- MODEL ----


type alias Model =
    { tiles : List Tile
    , key : Nav.Key
    , url : Url.Url
    }


type alias Tile =
    { value : Int
    , column : Int
    , row : Int
    }


init : () -> Url.Url -> Nav.Key -> ( Model, Cmd Msg )
init flags url key =
    ( initialModel url key, addTile )


initialModel : Url.Url -> Nav.Key -> Model
initialModel url key =
    { tiles = [], url = url, key = key }


addTile : Cmd Msg
addTile =
    Random.generate AddTile tileGenerator


tileGenerator : Random.Generator Tile
tileGenerator =
    Random.map2
        (\column row -> Tile 2 column row)
        (Random.int 1 4)
        (Random.int 1 4)



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
            , addTile
            )

        AddTile tile ->
            ( { model | tiles = [ tile ] }
            , Cmd.none
            )

        MoveUp ->
            ( { model | tiles = moveUp model.tiles }
            , Cmd.none
            )

        MoveDown ->
            ( { model | tiles = moveDown model.tiles }
            , Cmd.none
            )

        MoveRight ->
            ( { model | tiles = moveRight model.tiles }
            , Cmd.none
            )

        MoveLeft ->
            ( { model | tiles = moveLeft model.tiles }
            , Cmd.none
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


moveUp : List Tile -> List Tile
moveUp tiles =
    List.map (\t -> { t | row = clamp 1 4 (t.row - spacesUp t) }) tiles


spacesUp : Tile -> Int
spacesUp tile =
    tile.row - 1


moveDown : List Tile -> List Tile
moveDown tiles =
    List.map (\t -> { t | row = clamp 1 4 (t.row + spacesDown t) }) tiles


spacesDown : Tile -> Int
spacesDown tile =
    4 - tile.row


moveLeft : List Tile -> List Tile
moveLeft tiles =
    List.map (\t -> { t | column = clamp 1 4 (t.column - spacesLeft t) }) tiles


spacesLeft : Tile -> Int
spacesLeft tile =
    tile.column - 1


moveRight : List Tile -> List Tile
moveRight tiles =
    List.map (\t -> { t | column = clamp 1 4 (t.column + spacesRight t) }) tiles


spacesRight : Tile -> Int
spacesRight tile =
    4 - tile.column



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


tileContainer : List Tile -> Html Msg
tileContainer tiles =
    div [ class "tile-container" ]
        (listOfTiles tiles)


listOfTiles : List Tile -> List (Html Msg)
listOfTiles tiles =
    List.map (\t -> singleTile t) tiles


singleTile : Tile -> Html Msg
singleTile aTile =
    div
        [ class <| tileClassStr aTile ]
        [ div [ class "tile-inner" ]
            [ text <| String.fromInt aTile.value ]
        ]


tileClassStr : Tile -> String
tileClassStr aTile =
    String.join " "
        [ "tile"
        , "tile-new"
        , "tile-" ++ String.fromInt aTile.value
        , "tile-position-"
            ++ String.fromInt aTile.column
            ++ "-"
            ++ String.fromInt aTile.row
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
