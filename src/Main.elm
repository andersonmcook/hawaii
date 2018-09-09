module Main exposing (main)

-- https://github.com/NashReact/hawaii-react-js/tree/master/danhodges%2Bjs/src

import Array
import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Http
import Json.Decode as Decode exposing (Decoder, field, succeed)
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
    { id : Int
    , name : String
    }


type alias Model =
    { correctAnswers : Int
    , gameState : GameState
    , islands : List Island
    , seconds : Int
    , selectedIsland : Island
    , wrongAnswers : Int
    }


type GameState
    = Playing
    | PostGame
    | PreGame


initialModel : Model
initialModel =
    { correctAnswers = 0
    , gameState = PreGame
    , islands = []
    , seconds = 0

    -- Nothing ? in the future ?
    , selectedIsland = Island 1 "HAWAII" -- will need to be random every time
    , wrongAnswers = 0
    }



-- UPDATE


type Msg
    = ChooseIsland Island
    | LoadIslands (Result Http.Error (List Island))
    | RandomizeIsland Int
    | StartGame
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

                lastIndex =
                    List.length model.islands - 1
            in
            ( updatedModel, randomizeIsland lastIndex )

        LoadIslands (Ok islands) ->
            ( { model | islands = islands }, Cmd.none )

        LoadIslands (Err error) ->
            -- we would display an error here
            ( model, Cmd.none )

        RandomizeIsland index ->
            let
                selectedIsland =
                    case
                        model.islands
                            |> Array.fromList
                            |> Array.get index
                    of
                        Just island ->
                            island

                        Nothing ->
                            Island 1 "HAWAII"
            in
            ( { model | selectedIsland = selectedIsland }, Cmd.none )

        StartGame ->
            let
                seconds =
                    30
            in
            ( { model
                | correctAnswers = 0
                , gameState = Playing
                , seconds = seconds
                , wrongAnswers = 0
              }
            , tickSecond seconds
            )

        Tick 0 ->
            ( { model | gameState = PostGame, seconds = 0 }, Cmd.none )

        Tick seconds ->
            ( { model | seconds = seconds }, tickSecond seconds )



-- COMMANDS


islandsUrl : String
islandsUrl =
    "http://localhost:3000/islands"


getIslands : Cmd Msg
getIslands =
    islandDecoder
        |> Decode.list
        |> Http.get islandsUrl
        |> Http.send LoadIslands


randomizeIsland : Int -> Cmd Msg
randomizeIsland max =
    Random.generate RandomizeIsland (Random.int 0 max)


tickSecond : Int -> Cmd Msg
tickSecond seconds =
    Process.sleep 1000
        |> Task.andThen (\_ -> Task.succeed (seconds - 1))
        |> Task.perform Tick



-- DECODERS / ENCODERS


islandDecoder : Decoder Island
islandDecoder =
    Decode.map2 Island
        (field "id" Decode.int)
        (field "name" Decode.string)



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
                button [ onClick StartGame ] [ text "Start/Reset" ]

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
        [ button [ onClick <| ChooseIsland island ] [ text <| .name <| island ]
        ]


viewButtonList : Model -> Html Msg
viewButtonList { gameState, islands } =
    ul [ classList [ ( "hide", gameState /= Playing ) ] ] (List.map viewButton islands)


viewIsland : Island -> Island -> Html Msg
viewIsland selectedIsland island =
    let
        lowercaseIsland =
            String.toLower island.name
    in
    div
        [ classList
            [ ( lowercaseIsland, True )
            , ( "border", island == selectedIsland )
            ]
        ]
        [ img [ alt island.name, src ("./img/" ++ lowercaseIsland ++ ".svg") ] [] ]


view : Model -> Html Msg
view ({ gameState, islands, selectedIsland } as model) =
    div [ class "app" ]
        [ div [ class "ocean" ] (List.map (viewIsland selectedIsland) islands)
        , div [ class "dashboard" ]
            [ viewScoreboard model
            , viewButtonList model
            ]
        ]



-- MAIN


main : Program () Model Msg
main =
    Browser.element
        { init = always ( initialModel, getIslands )
        , subscriptions = always Sub.none
        , update = update
        , view = view
        }
