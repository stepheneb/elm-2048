module Main exposing (Model, Msg(..), init, main, update, view)

import Browser
import Html exposing (Html, a, div, h1, hr, img, p, strong, text)
import Html.Attributes exposing (class, href, src, target)



---- MODEL ----


type alias Model =
    {}


init : ( Model, Cmd Msg )
init =
    ( {}, Cmd.none )



---- UPDATE ----


type Msg
    = NoOp


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    ( model, Cmd.none )



---- VIEW ----


view : Model -> Html Msg
view model =
    div [ class "container" ]
        [ div [ class "heading" ]
            [ h1 [ class "title" ]
                [ text "2048" ]
            , div [ class "scores-container" ]
                [ div [ class "score-container" ]
                    [ text "0" ]
                , div [ class "best-container" ]
                    [ text "0" ]
                ]
            ]
        , div [ class "above-game" ]
            [ p [ class "game-intro" ]
                [ text "Join the numbers and get to the "
                , strong []
                    [ text "2048 tile!" ]
                ]
            , a [ class "restart-button" ]
                [ text "New Game" ]
            ]
        , div [ class "game-container" ]
            [ div [ class "game-message" ]
                [ p []
                    []
                , div [ class "lower" ]
                    [ a [ class "keep-playing-button" ]
                        [ text "Keep going" ]
                    , a [ class "retry-button" ]
                        [ text "Try again" ]
                    ]
                ]
            , div [ class "grid-container" ]
                [ div [ class "grid-row" ]
                    [ div [ class "grid-cell" ]
                        []
                    , div [ class "grid-cell" ]
                        []
                    , div [ class "grid-cell" ]
                        []
                    , div [ class "grid-cell" ]
                        []
                    ]
                , div [ class "grid-row" ]
                    [ div [ class "grid-cell" ]
                        []
                    , div [ class "grid-cell" ]
                        []
                    , div [ class "grid-cell" ]
                        []
                    , div [ class "grid-cell" ]
                        []
                    ]
                , div [ class "grid-row" ]
                    [ div [ class "grid-cell" ]
                        []
                    , div [ class "grid-cell" ]
                        []
                    , div [ class "grid-cell" ]
                        []
                    , div [ class "grid-cell" ]
                        []
                    ]
                , div [ class "grid-row" ]
                    [ div [ class "grid-cell" ]
                        []
                    , div [ class "grid-cell" ]
                        []
                    , div [ class "grid-cell" ]
                        []
                    , div [ class "grid-cell" ]
                        []
                    ]
                ]
            , div [ class "tile-container" ]
                []
            ]
        , p [ class "game-explanation" ]
            [ strong [ class "important" ]
                [ text "How to play: " ]
            , text "Use your "
            , strong []
                [ text "arrow keys" ]
            , text " to move the tiles. When two tiles with the same number touch, they "
            , strong []
                [ text "merge into one!" ]
            ]
        , hr []
            []
        , p []
            [ strong [ class "important" ]
                [ text "Note: " ]
            , text "This is not the official version of 2048! It is an Elm implementation of Gabriele Cirulli's "
            , a [ href "https://github.com/gabrielecirulli/2048" ]
                [ text "2048 game" ]
            ]
        , hr []
            []
        , p []
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
        ]



---- PROGRAM ----


main : Program () Model Msg
main =
    Browser.element
        { view = view
        , init = \_ -> init
        , update = update
        , subscriptions = always Sub.none
        }
