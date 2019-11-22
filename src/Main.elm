module Main exposing (main)

import Bootstrap.CDN as CDN
import Bootstrap.Grid as Grid
import Round
import Array exposing (..)
import GraphicSVG exposing (..)
import GraphicSVG.EllieApp exposing (..)
import List
import ShapeCreateAssets exposing (..)

main =
    gameApp Tick
        { model = init -- init is the value in the field model
        , title = "Shape Creator"
        , view = view
        , update = update
        }


init =
    { currentPage = 1,
    amplitude = 1,
    maxAmplitude = 10,
    minAmplitude = 1,
    frequency = 1,
    minFrequency = 0.1
    }


view model =
    collage 700 500
    [
        polygon [ ( -6, 0 ), ( 6, 0 ), ( 0, 12 ) ] |> filled red |> move (-250, -150) |> notifyTap AmplitudeUp,
        polygon [ ( -6, 0 ), ( 6, 0 ), ( 0, -12 ) ] |> filled red |> move (-250, -170) |> notifyTap AmplitudeDown,
        polygon [ ( -6, 0 ), ( 6, 0 ), ( 0, 12 ) ] |> filled red |> move (-220, -150) |> notifyTap FrequencyUp,
        polygon [ ( -6, 0 ), ( 6, 0 ), ( 0, -12 ) ] |> filled red |> move (-220, -170) |> notifyTap FrequencyDown,
        text (String.fromInt model.amplitude ++ " Sin (" ++ String.fromFloat model.frequency ++ "𝜋)") |> size 10 |> centered |> filled red |> move (-230, -160)
    ]


update msg model =
    case msg of
        Tick t _ ->
            { model | currentPage = 2 }
        AmplitudeUp ->
            { model | amplitude = 
                if model.amplitude < model.maxAmplitude then
                    model.amplitude + 1 
                else
                    model.amplitude }
        AmplitudeDown ->
            { model | amplitude = 
                if model.amplitude > model.minAmplitude then
                    model.amplitude - 1
                else
                    model.amplitude }
        FrequencyUp ->
            { model | frequency = Maybe.withDefault 0 (String.toFloat (Round.round 1 (model.frequency + 0.1))) }
        FrequencyDown ->
            { model | frequency = 
                if model.frequency > model.minFrequency then
                    Maybe.withDefault 0 (String.toFloat (Round.round 1 (model.frequency - 0.1))) 
                else
                    model.frequency }
        --{ model | amplitude = 2}

type Msg m1
    = Tick Float GetKeyState
    | AmplitudeUp
    | AmplitudeDown
    | FrequencyUp
    | FrequencyDown