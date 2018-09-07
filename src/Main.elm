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


type alias Island =
    String


type alias Model =
    { correctAnswers : Int
    , seconds : Int
    , selectedIsland : Island
    , wrongAnswers : Int
    }


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
    , seconds = 0
    , selectedIsland = "Hawaii" -- will need to be random every time
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
                        Just island ->
                            island

                        Nothing ->
                            "Hawaii"
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


viewScoreboard : Model -> Html Msg
viewScoreboard { correctAnswers, seconds, wrongAnswers } =
    let
        welcomeOrScoreboard =
            if correctAnswers + wrongAnswers + seconds == 0 then
                h3 [] [ text "Welcome to Pick An Island!" ]

            else
                div []
                    [ div [] [ text <| "Correct: " ++ String.fromInt correctAnswers ]
                    , div [] [ text <| "Wrong: " ++ String.fromInt wrongAnswers ]
                    ]

        buttonOrSeconds =
            if seconds == 0 then
                button [ onClick StartTimer ] [ text "Start/Reset" ]

            else
                div [] [ text <| String.fromInt seconds ++ " seconds" ]
    in
    div []
        [ welcomeOrScoreboard
        , buttonOrSeconds
        , p [] [ text "When the timer starts, click the name that corresponds to the island with a red border." ]
        ]



-- viewButton
-- viewIsland is temp
-- have to load the svg from public/static file
-- islandSvg : String -> String
-- islandSvg island =
--     "http://localhost:8001/" ++ ""


viewButton : Island -> Html Msg
viewButton island =
    li []
        [ button [ onClick <| ChooseIsland island ] [ text island ]
        ]


viewButtonList : Int -> Html Msg
viewButtonList seconds =
    ul [ classList [ ( "hide", seconds /= 0 ) ] ] (List.map viewButton islands)


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
view ({ seconds, selectedIsland } as model) =
    div [ class "app" ]
        -- [ div [ class "ocean" ] [ viewIslands selectedIsland ]
        [ div [ class "ocean" ] (List.map (viewIsland selectedIsland) islands)
        , div [ class "dashboard" ]
            [ viewScoreboard model
            , viewButtonList seconds
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
