module Main exposing (main)

-- https://github.com/NashReact/hawaii-react-js/tree/master/danhodges%2Bjs/src

import Array
import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Process
import Random
import Task



-- import Http
-- import Json.Decode as Decode exposing (Decoder, field, oneOf, succeed)
-- import Json.Decode.Pipeline exposing (hardcoded, optional, required)
-- import Json.Encode as Encode
-- INIT
-- MODEL


type alias Island =
    String


type alias Model =
    { correctAnswers : Int
    , gameState : GameState
    , seconds : Int
    , selectedIsland : Island
    , wrongAnswers : Int
    }


type GameState
    = Playing
    | PostGame
    | PreGame


islands : List Island
islands =
    [ "Hawaii"
    , "Kahoolawe"
    , "Kauai"
    , "Lanai"
    , "Maui"
    , "Molokai"
    , "Niihau"
    , "Oahu"
    ]


initialModel : Model
initialModel =
    { correctAnswers = 0
    , gameState = PreGame
    , seconds = 0
    , selectedIsland = "Hawaii" -- will need to be random every time
    , wrongAnswers = 0
    }



-- UPDATE


type Msg
    = ChooseIsland Island
    | RandomizeIsland Int
    | Play
    | Tick Int


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ChooseIsland island ->
            let
                updatedModel =
                    if island == model.selectedIsland then
                        { model | correctAnswers = model.correctAnswers + 1 }

                    else
                        { model | wrongAnswers = model.wrongAnswers + 1 }
            in
            ( updatedModel, randomizeIsland )

        RandomizeIsland index ->
            let
                selectedIsland =
                    case
                        islands
                            |> Array.fromList
                            |> Array.get index
                    of
                        Just island ->
                            island

                        Nothing ->
                            "Hawaii"
            in
            ( { model | selectedIsland = selectedIsland }, Cmd.none )

        Play ->
            -- could use initialModel in here instead of model
            -- like in example app
            -- will want to run a command that updates `seconds` every second
            ( { model | gameState = Playing, seconds = 30 }, tickSecond 30 )

        Tick 0 ->
            ( { model | gameState = PostGame, seconds = 0 }, Cmd.none )

        Tick seconds ->
            ( { model | seconds = seconds }, tickSecond seconds )



-- COMMANDS


randomizeIsland : Cmd Msg
randomizeIsland =
    Random.generate RandomizeIsland (Random.int 0 (List.length islands - 1))


tickSecond : Int -> Cmd Msg
tickSecond seconds =
    Process.sleep 1000
        |> Task.andThen (\_ -> Task.succeed (seconds - 1))
        |> Task.perform Tick



-- DECODERS / ENCODERS
-- VIEW
-- refactor this


viewScoreboard : Model -> Html Msg
viewScoreboard ({ correctAnswers, gameState, seconds, wrongAnswers } as model) =
    let
        welcomeOrScoreboard =
            if gameState == PreGame then
                h3 [] [ text "Welcome to Pick An Island!" ]

            else
                div []
                    [ div [] [ text <| "Correct: " ++ String.fromInt correctAnswers ]
                    , div [] [ text <| "Wrong: " ++ String.fromInt wrongAnswers ]
                    ]

        buttonOrSeconds =
            if gameState /= Playing then
                button [ onClick Play ] [ text "Start/Reset" ]

            else
                div [] [ text <| String.fromInt seconds ++ " seconds" ]
    in
    div []
        [ welcomeOrScoreboard
        , buttonOrSeconds
        , p [] [ text "When the timer starts, click the name that corresponds to the island with a red border." ]
        ]


viewButton : Island -> Html Msg
viewButton island =
    li []
        [ button [ onClick <| ChooseIsland island ] [ text island ]
        ]


viewButtonList : GameState -> Html Msg
viewButtonList gameState =
    ul [ classList [ ( "hide", gameState /= Playing ) ] ] (List.map viewButton islands)


viewIsland : Island -> Island -> Html Msg
viewIsland selectedIsland island =
    let
        lowercaseIsland =
            String.toLower island
    in
    div
        [ classList
            [ ( lowercaseIsland, True )
            , ( "border", island == selectedIsland )
            ]
        ]
        [ img [ alt island, src ("./img/" ++ lowercaseIsland ++ ".svg") ] [] ]


view : Model -> Html Msg
view ({ gameState, selectedIsland } as model) =
    div [ class "app" ]
        [ div [ class "ocean" ] (List.map (viewIsland selectedIsland) islands)
        , div [ class "dashboard" ]
            [ viewScoreboard model
            , viewButtonList gameState
            ]
        ]



-- MAIN


main : Program () Model Msg
main =
    Browser.element
        { init = always ( initialModel, Cmd.none )
        , subscriptions = always Sub.none
        , update = update
        , view = view
        }
