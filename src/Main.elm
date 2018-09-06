module Main exposing (main)

-- https://github.com/NashReact/hawaii-react-js/tree/master/danhodges%2Bjs/src

import Array
import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Random



-- import Http
-- import Json.Decode as Decode exposing (Decoder, field, oneOf, succeed)
-- import Json.Decode.Pipeline exposing (hardcoded, optional, required)
-- import Json.Encode as Encode
-- INIT
-- MODEL


type Island
    = Hawaii
    | Kahoolawe
    | Kauai
    | Lanai
    | Maui
    | Molokai
    | Niihau
    | Oahu


type alias Model =
    { correctAnswers : Int
    , seconds : Int
    , selectedIsland : Island
    , wrongAnswers : Int
    }



-- move islands closer to view


islands : List ( Island, String )
islands =
    [ ( Hawaii, "Hawaii" )
    , ( Kahoolawe, "Kahoolawe" )
    , ( Kauai, "Kauai" )
    , ( Lanai, "Lanai" )
    , ( Maui, "Maui" )
    , ( Molokai, "Molokai" )
    , ( Niihau, "Niihau" )
    , ( Oahu, "Oahu" )
    ]


initialModel : Model
initialModel =
    { correctAnswers = 0
    , seconds = 0
    , selectedIsland = Hawaii -- will need to be random every time
    , wrongAnswers = 0
    }



-- UPDATE


type Msg
    = ChooseIsland Island
    | RandomizeIsland Int
    | StartTimer


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
                        Just ( island, _ ) ->
                            island

                        Nothing ->
                            Hawaii
            in
            ( { model | selectedIsland = selectedIsland }, Cmd.none )

        StartTimer ->
            -- could use initialModel in here instead of model
            -- like in example app
            -- will want to run a command that updates `seconds` every second
            ( { model | seconds = 30 }, Cmd.none )



-- COMMANDS


randomizeIsland : Cmd Msg
randomizeIsland =
    Random.generate RandomizeIsland (Random.int 0 (List.length islands - 1))



-- DECODERS / ENCODERS
-- VIEW


viewIslands : Island -> Html Msg
viewIslands island =
    div [] [ text "whatever this is supposed to be" ]


viewScoreboard : Model -> Html Msg
viewScoreboard { correctAnswers, seconds, wrongAnswers } =
    div [] [ text "some scoreboard with a button and onClick for StartTimer" ]



-- viewButton
-- viewIsland is temp
-- have to load the svg from public/static file
-- islandSvg : String -> String
-- islandSvg island =
--     "http://localhost:8001/" ++ ""


viewButton : ( Island, String ) -> Html Msg
viewButton ( island, islandString ) =
    button [ onClick <| ChooseIsland island ] [ text islandString ]


viewIsland : Island -> ( Island, String ) -> Html Msg
viewIsland selectedIsland ( island, islandString ) =
    div
        [ classList
            [ ( String.toLower islandString, True )
            , ( "border", island == selectedIsland )
            ]
        ]
        [ img [ alt islandString, src "" ] [] ]


view : Model -> Html Msg
view ({ selectedIsland } as model) =
    div [ class "App" ]
        [ div [ class "ocean" ] [ viewIslands selectedIsland ]
        , div [ class "dashboard" ] [ viewScoreboard model ]
        , div [] (List.map viewButton islands)
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
